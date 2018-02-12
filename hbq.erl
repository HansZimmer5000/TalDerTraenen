%%%-------------------------------------------------------------------
%%% @author Arne Thiele & Michael Müller
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 13:29
%%%-------------------------------------------------------------------
-module(hbq).
-author("Arne Thiele & Michael Müller").

%% API
-export([start/1]).

-define(HBQLOG, 'hbq.log').
-define(DLQLIMIT, extractValueFromConfig(dlqlimit)).

%------------------------------------------------------------------------------------------------------
%																					>>INIT UND LOOPS<<
%------------------------------------------------------------------------------------------------------

%Methode um den HBQ-Prozess an sich anzustoßen
start(Name) ->
  log_status(start, "\n\n\nHBQ wird gestartet"),
  HBQPID = spawn(fun() -> waitForInit(?DLQLIMIT) end),
  register(Name, HBQPID),
  HBQPID.

waitForInit(DLQLimit) ->
  %Receive Block für die Initialisierung der HBQ
  receive
  %Initialisiert die intern verwendete Listenstruktur
    {PID, {request, initHBQ}} ->
      DLQ = dlq:initDLQ(DLQLimit,'dlq.log'),
      initHBQHandler(PID), work([], DLQ, DLQLimit);
    Any ->
      log_status(waitForInit, io_lib:format("ERR: Etwas erhalten, was nicht zugeordnet werden konnte ~p", [Any])),
      waitForInit(DLQLimit)
  end.

work(HoldbackQueue, DeliveryQueue, DLQLimit) ->
  %Receive Block für die Schnittstellen der HBQ
  receive
  %Sortiert eine neu erhaltene Nachricht in die Listenstruktur ein
    {PID, {request, pushHBQ, MessageAsList}} ->
      {NewHBQ, NewDLQ} = pushHBQHandler(PID, MessageAsList, HoldbackQueue, DeliveryQueue, DLQLimit),
      work(NewHBQ, NewDLQ, DLQLimit);

  %Delegationsmethode an die DLQ, damit diese die spezifizierte Nachricht an einen spezifizierten Client ausliefert
    {PID, {request, deliverMSG, NNr, ToClient}} ->
      deliverMSGHandler(PID, NNr, ToClient, DeliveryQueue),
      work(HoldbackQueue, DeliveryQueue, DLQLimit);

  %An dieser Stelle kein weiterer rekursiver Aufruf, weil die HBQ terminieren soll
    {PID, {request, dellHBQ}} -> deleteHBQHandler(PID, DeliveryQueue);

    Any ->
      log_status(work, io_lib:format("ERR: Etwas erhalten, was nicht zugeordnet werden konnte ~p", [Any])),
      work(HoldbackQueue, DeliveryQueue, DLQLimit)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

%Bestätigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
initHBQHandler(PID) ->
  PID ! {reply, ok}.

pushHBQHandler(PID, [NNr, MessageLST, TSClientout], HoldbackQueue, DeliveryQueue, DLQLimit) ->
  % 1) Prüfen ob die geschickte Nachrichtennummer bereits auslieferbar ist
  log_status(pushHBQHandler, io_lib:format("input: PID: ~p, MsgNr: ~p", [PID, NNr])),
  TShbqin = erlang:timestamp(),
  IsFuture = werkzeug:lessTS(TShbqin,TSClientout),
  log_status(pushHBQHandler, io_lib:format("Kommt die Nachricht aus der Zukunft? ~p",[IsFuture])),
  MessageWithAddedTS = [NNr, MessageLST, TSClientout, TShbqin],
  CanBeDelivered = isInOrder(MessageWithAddedTS, DeliveryQueue),
  log_status(pushHBQHandler, io_lib:format("Message CanBeDelivered? ~p", [CanBeDelivered])),
  % 2) falls ja --> mittels push2DLQ ausliefern und Reihenfolge der bereits gespeicherten Nachrichten prüfen; ggf weitere Nachrichten an die DLQ schicken
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(MessageWithAddedTS, DeliveryQueue, ?HBQLOG), 
      {NewHBQ, ResultDLQ} = checkNextMSGAndPush(HoldbackQueue, NewDLQ),
      log_status(pushHBQHandler, io_lib:format("HoldbackQueue Nach checkNextMSGAndPush: ~p",[NewHBQ])),
      Result = {NewHBQ, ResultDLQ};
  % 3) falls nein --> Nachricht in die HBQ einsortieren (den Nummern nach aufsteigend)
    true ->
      NewHBQ = insertIntoHBQ(MessageWithAddedTS, HoldbackQueue),
      % Limit abprüfen (2/3el der DLQ) und ggf Lücke schließen
      NewDLQ = checkLimitAndCloseGap(NewHBQ, DeliveryQueue, DLQLimit),
      {ResultHBQ,ResultDLQ} = checkNextMSGAndPush(NewHBQ,NewDLQ),
      log_status(pushHBQHandler, io_lib:format("HoldbackQueue Nach checkNextMSGAndPush: ~p",[ResultHBQ])),
      Result = {ResultHBQ,ResultDLQ}
  end,
  PID ! {reply, ok},
  Result.

%Handler für den deliverMSG Befehl. Dieser wird an die dlq delegiert, anschließend wird die Nummer der versendeten Nachricht an den
%Server zurückgegeben
deliverMSGHandler(PID, NNr, ToClient, DLQ) ->
  log_status(deliverMSGHandler, io_lib:format("input: PID: ~p, MsgNr: ~p, ToClient: ~p", [PID, NNr, ToClient])),
  SentMsgNum = dlq:deliverMSG(NNr,ToClient,DLQ, ?HBQLOG),
  log_status(deliverMSGHandler, io_lib:format("Number of sent Message: ~p", [SentMsgNum])),
  PID ! {reply,SentMsgNum}.

%Handler für den delete Befehl, löscht auch die DLQ
deleteHBQHandler(PID, DLQ) ->
  AtomAnswer = dlq:delDLQ(DLQ),
  if 
    (AtomAnswer == ok) ->
      log_status(deleteHBQHandler,"DLQ wurde erfolgreich gelöscht");
    true -> 
      log_status(deleteHBQHandler,"ERR: DLQ wurde NICHT gelöscht")
  end,
  PID ! {reply, ok},
  exit(self(),kill).

%------------------------------------------------------------------------------------------------------
%																					>>HILFSMETHODEN<<
%------------------------------------------------------------------------------------------------------

%Hilfsmethode um zu prüfen, ob die DLQ diese Nachricht erwartet
isInOrder([NNr | _], DLQ) ->
  log_status(isInOrder, io_lib:format("input: NNr: ~p", [NNr])),
  ExpectedNNr = dlq:expectedNr(DLQ),
  IsExpected = (ExpectedNNr == NNr),
  log_status(isInOrder, io_lib:format("IsExpected? ~p", [IsExpected])),
  IsExpected.

%Rekursive Hilfsmethode, welche nach und nach weitere Nachrichten an die DLQ schickt, falls möglich
checkNextMSGAndPush([], DLQ) ->
  log_status(checkNextMSGAndPush, "Leeres Array"),
  {[], DLQ};
checkNextMSGAndPush([HBQHead | HBQTail], DLQ) ->
  log_status(checkNextMSGAndPush, io_lib:format("Aktueller Head: ~p", [HBQHead])),
  CanBeDelivered = isInOrder(HBQHead, DLQ),
  log_status(checkNextMSGAndPush, io_lib:format("Message CanBeDelivered? ~p", [CanBeDelivered])),
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(HBQHead, DLQ, ?HBQLOG),
      checkNextMSGAndPush(HBQTail, NewDLQ);
    true -> 
      {[HBQHead | HBQTail], DLQ}
  end.

insertIntoHBQ(Message, []) ->
  log_status(insertIntoHBQ, "HBQ ist leer, Element wird einfach eingefügt"),
  [Message];
insertIntoHBQ(Message, HBQ) ->
  log_status(insertIntoHBQ, "HBQ ist nicht leer"),
  NewHBQ = insertIntoHBQ_([], Message, HBQ),
  NewHBQ.

%Prüft pro Rekursionsschritt, ob das forderste Element der HBQ eine kleinere NNR hat als das einzufügende Element.
%Wenn dem nicht so ist, wird das erste Element der HBQ in den Akku geschrieben und es erfolgt ein weiterer Rekursionsaufruf
%Wenn dem so ist, wird das Neue Element an den Akku gehängt, dieser vor den Rest der HBQ gehängt und das ganze zurückgegeben ausgegeben.
insertIntoHBQ_(Akku, Message, []) ->
  log_status(insertIntoHBQ_, "ist ganz durchgelaufen, element wird ganz hinten angehängt"),
  NewHBQ = lists:append(Akku, [Message]),
  NewHBQ;
insertIntoHBQ_(Akku, [NNr | MessageRest], [[HBQHeadNNr | HBQHeadRest] | HBQTail]) ->
  log_status(insertIntoHBQ_, io_lib:format("input: NachrichtenNummer: ~p, AktuelleHBQNummer: ~p", [NNr, HBQHeadNNr])),
  IsSmaller = NNr < HBQHeadNNr,
  if (IsSmaller) ->
    %Hier war der Fehler :)
    NewFirstPartOfHBQ = lists:append(Akku, [[NNr | MessageRest]]),
    RestOfHBQ = [[HBQHeadNNr | HBQHeadRest] | HBQTail],
    NewHBQ = lists:append(NewFirstPartOfHBQ, RestOfHBQ);
    true ->
      NewAkku = lists:append(Akku, [[HBQHeadNNr | HBQHeadRest]]),
      NewHBQ = insertIntoHBQ_(NewAkku, [NNr | MessageRest], HBQTail)
  end,
  NewHBQ.

%Hilfsmethode zum Überprüfen der Einhaltung des 2/3el Ansatzes
checkLimitAndCloseGap(HBQ, DLQ, DLQLimit) ->
  log_start(checkLimitAndCloseGap),
  GapLimit = DLQLimit / 3 * 2,
  HBQSize = length(HBQ),
  if
    (HBQSize > GapLimit) ->
      log_status(checkLimitAndCloseGap,"2/3 Regel erfüllt"),
      %Ist das Limit überschritten, so wird die erste Lücke geschlossen und an die DLQ weitergegeben
      NewDLQ = searchForGapAndClose(HBQ, DLQ),
      NewDLQ;
    true ->
      %Ist das Limit nicht überschritten, bleiben die beiden Queues unverändert
      log_status(checkLimitAndCloseGap,"2/3 Regel nicht erfüllt"),
      DLQ
  end.

% Hilfsmethode die herausfindet wo genau die Lücke ist und die Lücke schließt.
searchForGapAndClose(HBQ, DLQ) ->
  log_start(searchForGapAndClose),
  % Wo ist die Lücke?
  {GapStartNNr,GapEndNNr} = searchForGap(HBQ,DLQ),
  % Erstelle Fehlernachricht und logge
  GapMessageList = createGapMessage(GapStartNNr,GapEndNNr),
  % Schließe diese Lücke
  NewDLQ = dlq:push2DLQ(GapMessageList, DLQ, ?HBQLOG),
  log_status(searchForGapAndClose,io_lib:format("newDLQ: ~p",[NewDLQ])),
  NewDLQ.

% Hilfsmethode die herausfindet wo genau die Lücke ist.
searchForGap([[ActualNNr|_]|_HBQRest],DLQ) ->
  log_status(searchForGap,io_lib:format("input: ActualNr: ~p",[ActualNNr])),
  ExpectedNNr = dlq:expectedNr(DLQ),
  log_status(searchForGap,io_lib:format("Gaprange: ~p-~p",[ExpectedNNr,ActualNNr-1])),
  {ExpectedNNr, ActualNNr - 1}.

% Erstellt eine Mock Nachricht um die Lücke schließen zu können.
createGapMessage(GapStartNNr,GapEndNNr) ->
  log_start(createGapMessage),
  TS = erlang:timestamp(),
  [GapEndNNr,[io_lib:format("Error Nachricht zum Lücke von ~p bis ~p zu füllen",[GapStartNNr, GapEndNNr])],TS,TS].

%------------------------------------------------------------------------------------------------------
%																					>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

% Holt aus der Configdatei (server.cfg) den benötigten Wert für den eingegebenen Key.
% Erwartet Configdatei mit "{key1,value1}. {key2,value2}. ....."
extractValueFromConfig(Key) ->
  %log_status(extractValueFromConfig,io_lib:format("Key: ~p",[Key])),
  {ok, ConfigListe} = file:consult("server.cfg"),
  {ok, Value} = werkzeug:get_config_value(Key, ConfigListe),
  Value.

% Logt start einer Funktion
log_start(Funktion) -> log_status(Funktion, io_lib:format("~p started. ~n", [Funktion])).

% Logt den aktuellen Status / Wert / ... einer Funktion.
log_status(Funktion, Status) ->
  werkzeug:logging(?HBQLOG, io_lib:format("~p ~p hat status: ~s. ~n", [werkzeug:timeMilliSecond(), Funktion, Status])).
