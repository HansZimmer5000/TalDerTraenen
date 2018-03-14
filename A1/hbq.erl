-module(hbq).

%% API
-export([
    start/0,
    waitForInit/0,
    initHBQHandler/1,

    receive_loop/2,

    pushHBQHandler/4,
    inHBQeinfuegen/2,
    deliverMSGHandler/4,
    deleteHBQHandler/2,

    isInOrder/2,
    pruefeNaechsteNachrichtUndPushe/2,
    pruefeLimitUndFuelleSpalte/3,
    sucheUndFuelleSpalte/2,
    sucheSpalte/2,
    erstelleSpaltNachricht/2
]).

% KOnSTANTEN
-define(CONFIG_FILENAME, "hbq.cfg").
-define(LOG_DATEI_NAME, 'hbq.log').
-define(DLQ_LOG_DATEI, 'dlq.log').
-define(HBQNAME, extractValueFromConfig(hbqname)).
-define(DLQLIMIT, extractValueFromConfig(dlqlimit)).

start() ->
  logge_status("HBQ wird gestartet"),
  HBQPID = spawn(fun() -> waitForInit() end),
  register(?HBQNAME, HBQPID),
  HBQPID.

waitForInit() ->
  %Receive Block für die Initialisierung der HBQ
  receive
  %Initialisiert die intern verwendete Listenstruktur
    {PID, {request, initHBQ}} ->
      DLQ = dlq:initDLQ(?DLQLIMIT, ?DLQ_LOG_DATEI),
      initHBQHandler(PID), 
      receive_loop([], DLQ)
  end.

receive_loop(HoldbackQueue, DeliveryQueue) ->
  %Receive Block für die Schnittstellen der HBQ
  receive
  %Sortiert eine neu erhaltene Nachricht in die Listenstruktur ein
    {PID, {request, pushHBQ, MessageAsList}} ->
      {NewHBQ, NewDLQ} = pushHBQHandler(PID, MessageAsList, HoldbackQueue, DeliveryQueue),
      receive_loop(NewHBQ, NewDLQ);

  %Delegationsmethode an die DLQ, damit diese die spezifizierte Nachricht an einen spezifizierten Client ausliefert
    {PID, {request, deliverMSG, NNr, ToClient}} ->
      deliverMSGHandler(PID, NNr, ToClient, DeliveryQueue),
      receive_loop(HoldbackQueue, DeliveryQueue);

  %An dieser Stelle kein weiterer rekursiver Aufruf, weil die HBQ terminieren soll
    {PID, {request, dellHBQ}} -> deleteHBQHandler(PID, DeliveryQueue)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

%Bestätigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
initHBQHandler(PID) ->
  PID ! {reply, ok}.

pushHBQHandler(PID, [NNr, MessageLST, TSClientout], HBQ, DLQ) ->
  % 1) Prüfen ob die geschickte Nachrichtennummer bereits auslieferbar ist
  logge_status(io_lib:format("pushHBQHandler PID: ~p, MsgNr: ~p", [PID, NNr])),
  TShbqin = erlang:timestamp(),
  IsFuture = vsutil:lessTS(TShbqin,TSClientout),
  logge_status(io_lib:format("Kommt die Nachricht aus der Zukunft? ~p",[IsFuture])),
  MessageWithAddedTS = [NNr, MessageLST, TSClientout, TShbqin],
  CanBeDelivered = isInOrder(MessageWithAddedTS, DLQ),
  logge_status(io_lib:format("Kann Nachricht gesendet werden? ~p", [CanBeDelivered])),
  % 2) falls ja --> mittels push2DLQ ausliefern und Reihenfolge der bereits gespeicherten Nachrichten prüfen; ggf weitere Nachrichten an die DLQ schicken
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(MessageWithAddedTS, DLQ, ?DLQ_LOG_DATEI), 
      {NewHBQ, ResultDLQ} = pruefeNaechsteNachrichtUndPushe(HBQ, NewDLQ),
      Result = {NewHBQ, ResultDLQ};
  % 3) falls nein --> Nachricht in die HBQ einsortieren (den Nummern nach aufsteigend)
    true ->
      NewHBQ = inHBQeinfuegen(MessageWithAddedTS, HBQ),
      % Limit abprüfen (2/3el der DLQ) und ggf Lücke schließen
      NewDLQ = pruefeLimitUndFuelleSpalte(NewHBQ, DLQ, ?DLQLIMIT),
      {ResultHBQ,ResultDLQ} = pruefeNaechsteNachrichtUndPushe(NewHBQ,NewDLQ),
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
pruefeNaechsteNachrichtUndPushe([], DLQ) ->
  logge_status("Leeres Array"),
  {[], DLQ};
pruefeNaechsteNachrichtUndPushe([HBQHead | HBQTail], DLQ) ->
 %logge_status(io_lib:format("Aktueller Head: ~p", [HBQHead])),
  CanBeDelivered = isInOrder(HBQHead, DLQ),
  logge_status(io_lib:format("Message CanBeDelivered? ~p", [CanBeDelivered])),
  case CanBeDelivered of
    true ->
      NewDLQ = dlq:push2DLQ(HBQHead, DLQ, ?DLQ_LOG_DATEI),
      pruefeNaechsteNachrichtUndPushe(HBQTail, NewDLQ);
    false -> 
      {[HBQHead | HBQTail], DLQ}
  end.

inHBQeinfuegen(Message, []) ->
  logge_status("HBQ ist leer, Element wird einfach eingefügt"),
  [Message];
inHBQeinfuegen(Message, HBQ) ->
  logge_status("HBQ ist nicht leer"),
  NewHBQ = inHBQeinfuegen_([], Message, HBQ),
  NewHBQ.

%Prüft pro Rekursionsschritt, ob das forderste Element der HBQ eine kleinere NNR hat als das einzufügende Element.
%Wenn dem nicht so ist, wird das erste Element der HBQ in den Akku geschrieben und es erfolgt ein weiterer Rekursionsaufruf
%Wenn dem so ist, wird das Neue Element an den Akku gehängt, dieser vor den Rest der HBQ gehängt und das ganze zurückgegeben ausgegeben.
inHBQeinfuegen_(Akku, Message, []) ->
  logge_status("ist ganz durchgelaufen, element wird ganz hinten angehängt"),
  NewHBQ = lists:append(Akku, [Message]),
  NewHBQ;
inHBQeinfuegen_(Akku, [NNr | MessageRest], [[HBQHeadNNr | HBQHeadRest] | HBQTail]) ->
  IsSmaller = NNr < HBQHeadNNr,
  if (IsSmaller) ->
    %Hier war der Fehler :)
    NewFirstPartOfHBQ = lists:append(Akku, [[NNr | MessageRest]]),
    RestOfHBQ = [[HBQHeadNNr | HBQHeadRest] | HBQTail],
    NewHBQ = lists:append(NewFirstPartOfHBQ, RestOfHBQ);
    true ->
      NewAkku = lists:append(Akku, [[HBQHeadNNr | HBQHeadRest]]),
      NewHBQ = inHBQeinfuegen_(NewAkku, [NNr | MessageRest], HBQTail)
  end,
  NewHBQ.

%Hilfsmethode zum Überprüfen der Einhaltung des 2/3el Ansatzes
pruefeLimitUndFuelleSpalte(HBQ, DLQ, DLQLimit) ->
  GapLimit = DLQLimit / 3 * 2,
  HBQSize = length(HBQ),
  if
    (HBQSize > GapLimit) ->
      logge_status("2/3 Regel erfüllt"),
      %Ist das Limit überschritten, so wird die erste Lücke geschlossen und an die DLQ weitergegeben
      NewDLQ = sucheUndFuelleSpalte(HBQ, DLQ),
      NewDLQ;
    true ->
      %Ist das Limit nicht überschritten, bleiben die beiden Queues unverändert
      logge_status("2/3 Regel nicht erfüllt"),
      DLQ
  end.

% Hilfsmethode die herausfindet wo genau die Lücke ist und die Lücke schließt.
sucheUndFuelleSpalte(HBQ, DLQ) ->
  % Wo ist die Lücke?
  {SpaltStartNNr, SpaltEndeNNr} = sucheSpalte(HBQ, DLQ),
  % Erstelle Fehlernachricht und logge
  GapMessageList = erstelleSpaltNachricht(SpaltStartNNr, SpaltEndeNNr),
  % Schließe diese Lücke
  NewDLQ = dlq:push2DLQ(GapMessageList, DLQ, ?LOG_DATEI_NAME),
  NewDLQ.

% Hilfsmethode die herausfindet wo genau die Lücke ist.
sucheSpalte([[AktuelleNNr | _AktuelleNachrichtRest] | _HBQRest],DLQ) ->
  ErwarteteNNr = dlq:expectedNr(DLQ),
  logge_status(io_lib:format("Gaprange: ~p-~p",[ErwarteteNNr, AktuelleNNr - 1])),
  {ErwarteteNNr, AktuelleNNr - 1}.

% Erstellt eine Mock Nachricht um die Lücke schließen zu können.
erstelleSpaltNachricht(SpaltStartNNr,SpaltEndeNNr) ->
  TS = erlang:timestamp(),
  [SpaltEndeNNr, lists:flatten(io_lib:format("Error Nachricht zum Luecke von ~p bis ~p zu fuellen", [SpaltStartNNr, SpaltEndeNNr])), TS, TS].

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
