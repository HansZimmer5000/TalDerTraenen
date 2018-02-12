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
-export([start/0]).

-define(CONFIG_FILENAME, "hbq.cfg").
-define(LOG_DATEI_NAME, 'hbq.log').
-define(DLQ_LOG_DATEI, 'dlq.log').
-define(HBQNAME, extractValueFromConfig(hbqname)).
-define(DLQLIMIT, extractValueFromConfig(dlqlimit)).

%------------------------------------------------------------------------------------------------------
%																					>>INIT UND LOOPS<<
%------------------------------------------------------------------------------------------------------

%Methode um den HBQ-Prozess an sich anzustoßen
start() ->
  logge_status("HBQ wird gestartet"),
  HBQPID = spawn(fun() -> waitForInit(?DLQLIMIT) end),
  register(?HBQNAME, HBQPID),
  HBQPID.

waitForInit(DLQLimit) ->
  %Receive Block für die Initialisierung der HBQ
  receive
  %Initialisiert die intern verwendete Listenstruktur
    {PID, {request, initHBQ}} ->
      DLQ = dlq:initDLQ(DLQLimit, ?DLQ_LOG_DATEI),
      initHBQHandler(PID), 
      work([], DLQ, DLQLimit)
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
    {PID, {request, dellHBQ}} -> deleteHBQHandler(PID, DeliveryQueue)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

%Bestätigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
initHBQHandler(PID) ->
  PID ! {reply, ok}.

pushHBQHandler(PID, [NNr, MessageLST, TSClientout], HoldbackQueue, DeliveryQueue, DLQLimit) ->
  % 1) Prüfen ob die geschickte Nachrichtennummer bereits auslieferbar ist
  logge_status(io_lib:format("pushHBQHandler PID: ~p, MsgNr: ~p", [PID, NNr])),
  TShbqin = erlang:timestamp(),
  IsFuture = vsutil:lessTS(TShbqin,TSClientout),
  logge_status(io_lib:format("Kommt die Nachricht aus der Zukunft? ~p",[IsFuture])),
  MessageWithAddedTS = [NNr, MessageLST, TSClientout, TShbqin],
  CanBeDelivered = isInOrder(MessageWithAddedTS, DeliveryQueue),
  logge_status(io_lib:format("Kann Nachricht gesendet werden? ~p", [CanBeDelivered])),
  % 2) falls ja --> mittels push2DLQ ausliefern und Reihenfolge der bereits gespeicherten Nachrichten prüfen; ggf weitere Nachrichten an die DLQ schicken
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(MessageWithAddedTS, DeliveryQueue, ?DLQ_LOG_DATEI), 
      {NewHBQ, ResultDLQ} = checkNextMSGAndPush(HoldbackQueue, NewDLQ),
      Result = {NewHBQ, ResultDLQ};
  % 3) falls nein --> Nachricht in die HBQ einsortieren (den Nummern nach aufsteigend)
    true ->
      NewHBQ = insertIntoHBQ(MessageWithAddedTS, HoldbackQueue),
      % Limit abprüfen (2/3el der DLQ) und ggf Lücke schließen
      NewDLQ = checkLimitAndCloseGap(NewHBQ, DeliveryQueue, DLQLimit),
      {ResultHBQ,ResultDLQ} = checkNextMSGAndPush(NewHBQ,NewDLQ),
      Result = {ResultHBQ,ResultDLQ}
  end,
  PID ! {reply, ok},
  Result.

%Handler für den deliverMSG Befehl. Dieser wird an die dlq delegiert, anschließend wird die Nummer der versendeten Nachricht an den
%Server zurückgegeben
deliverMSGHandler(PID, NNr, ToClient, DLQ) ->
  SentMsgNum = dlq:deliverMSG(NNr,ToClient,DLQ, ?DLQ_LOG_DATEI),
  logge_status(io_lib:format("Number of sent Message: ~p", [SentMsgNum])),
  PID ! {reply,SentMsgNum}.

%Handler für den delete Befehl, löscht auch die DLQ
deleteHBQHandler(PID, DLQ) ->
  AtomAnswer = dlq:delDLQ(DLQ),
  if 
    (AtomAnswer == ok) ->
      logge_status("DLQ wurde erfolgreich gelöscht");
    true -> 
      logge_status("ERR: DLQ wurde NICHT gelöscht")
  end,
  PID ! {reply, ok},
  exit(self(),kill).

%------------------------------------------------------------------------------------------------------
%																					>>HILFSMETHODEN<<
%------------------------------------------------------------------------------------------------------

%Hilfsmethode um zu prüfen, ob die DLQ diese Nachricht erwartet
isInOrder([NNr | _], DLQ) ->
  ExpectedNNr = dlq:expectedNr(DLQ),
  ExpectedNNr == NNr.

%Rekursive Hilfsmethode, welche nach und nach weitere Nachrichten an die DLQ schickt, falls möglich
checkNextMSGAndPush([], DLQ) ->
  logge_status("Leeres Array"),
  {[], DLQ};
checkNextMSGAndPush([HBQHead | HBQTail], DLQ) ->
 logge_status(io_lib:format("Aktueller Head: ~p", [HBQHead])),
  CanBeDelivered = isInOrder(HBQHead, DLQ),
  logge_status(io_lib:format("Message CanBeDelivered? ~p", [CanBeDelivered])),
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(HBQHead, DLQ, ?DLQ_LOG_DATEI),
      checkNextMSGAndPush(HBQTail, NewDLQ);
    true -> 
      {[HBQHead | HBQTail], DLQ}
  end.

insertIntoHBQ(Message, []) ->
  logge_status("HBQ ist leer, Element wird einfach eingefügt"),
  [Message];
insertIntoHBQ(Message, HBQ) ->
  logge_status("HBQ ist nicht leer"),
  NewHBQ = insertIntoHBQ_([], Message, HBQ),
  NewHBQ.

%Prüft pro Rekursionsschritt, ob das forderste Element der HBQ eine kleinere NNR hat als das einzufügende Element.
%Wenn dem nicht so ist, wird das erste Element der HBQ in den Akku geschrieben und es erfolgt ein weiterer Rekursionsaufruf
%Wenn dem so ist, wird das Neue Element an den Akku gehängt, dieser vor den Rest der HBQ gehängt und das ganze zurückgegeben ausgegeben.
insertIntoHBQ_(Akku, Message, []) ->
  logge_status("ist ganz durchgelaufen, element wird ganz hinten angehängt"),
  NewHBQ = lists:append(Akku, [Message]),
  NewHBQ;
insertIntoHBQ_(Akku, [NNr | MessageRest], [[HBQHeadNNr | HBQHeadRest] | HBQTail]) ->
  logge_status(io_lib:format("input: NachrichtenNummer: ~p, AktuelleHBQNummer: ~p", [NNr, HBQHeadNNr])),
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
  GapLimit = DLQLimit / 3 * 2,
  HBQSize = length(HBQ),
  if
    (HBQSize > GapLimit) ->
      logge_status("2/3 Regel erfüllt"),
      %Ist das Limit überschritten, so wird die erste Lücke geschlossen und an die DLQ weitergegeben
      NewDLQ = searchForGapAndClose(HBQ, DLQ),
      NewDLQ;
    true ->
      %Ist das Limit nicht überschritten, bleiben die beiden Queues unverändert
      logge_status("2/3 Regel nicht erfüllt"),
      DLQ
  end.

% Hilfsmethode die herausfindet wo genau die Lücke ist und die Lücke schließt.
searchForGapAndClose(HBQ, DLQ) ->
  % Wo ist die Lücke?
  {GapStartNNr,GapEndNNr} = searchForGap(HBQ,DLQ),
  % Erstelle Fehlernachricht und logge
  GapMessageList = createGapMessage(GapStartNNr,GapEndNNr),
  % Schließe diese Lücke
  NewDLQ = dlq:push2DLQ(GapMessageList, DLQ, ?LOG_DATEI_NAME),
  NewDLQ.

% Hilfsmethode die herausfindet wo genau die Lücke ist.
searchForGap([[ActualNNr|_]|_HBQRest],DLQ) ->
  ExpectedNNr = dlq:expectedNr(DLQ),
  logge_status(io_lib:format("Gaprange: ~p-~p",[ExpectedNNr,ActualNNr-1])),
  {ExpectedNNr, ActualNNr - 1}.

% Erstellt eine Mock Nachricht um die Lücke schließen zu können.
createGapMessage(GapStartNNr,GapEndNNr) ->
  TS = erlang:timestamp(),
  [GapEndNNr,[io_lib:format("Error Nachricht zum Lücke von ~p bis ~p zu füllen",[GapStartNNr, GapEndNNr])],TS,TS].

%------------------------------------------------------------------------------------------------------
%																					>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

% Holt aus der Configdatei (server.cfg) den benötigten Wert für den eingegebenen Key.
% Erwartet Configdatei mit "{key1,value1}. {key2,value2}. ....."
extractValueFromConfig(Key) ->
  {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
  {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
  Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).

logge_nachricht_status(Nachricht, Status) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht).
