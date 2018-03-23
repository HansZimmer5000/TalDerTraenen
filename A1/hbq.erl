-module(hbq).

% API
-export([
    start/0,
    wait_for_init/0,
    init_hbq_handler/1,

    receive_loop/2,

    push_hbq_handler/4,
    in_hbq_einfuegen/2,
    deliver_nachricht_handler/4,
    delete_hbq_handler/2,

    is_in_order/2,
    pruefe_naechste_nachricht_und_pushe/2,
    pruefe_limit_und_fuelle_spalte/3,
    suche_und_fuelle_spalte/2,
    suche_spalte/2,
    erstelle_spalt_nachricht/2
]).

% KONSTANTEN
-define(CONFIG_FILENAME, "server.cfg").
-define(LOG_DATEI_NAME, 'hbq.log').
-define(DLQ_LOG_DATEI, 'dlq.log').
-define(HBQNAME, extractValueFromConfig(hbqname)).
-define(DLQLIMIT, extractValueFromConfig(dlqlimit)).

%------------------------------------------------------------------------------------------------------
%																	>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start() ->
  logge_status("HBQ wird gestartet"),
  HBQPID = spawn(fun() -> wait_for_init() end),
  register(?HBQNAME, HBQPID),
  HBQPID.

wait_for_init() ->
  receive
    {PID, {request, initHBQ}} ->
      io:fwrite("got init request"),
      DLQ = dlq:initDLQ(?DLQLIMIT, ?DLQ_LOG_DATEI),
      init_hbq_handler(PID), 
      receive_loop([], DLQ)
  end.

receive_loop(HoldbackQueue, DeliveryQueue) ->
  io:fwrite("receive_loop"),
  receive
    {PID, {request, pushHBQ, MessageAsList}} ->
      {NewHBQ, NewDLQ} = push_hbq_handler(PID, MessageAsList, HoldbackQueue, DeliveryQueue),
      receive_loop(NewHBQ, NewDLQ);

    {PID, {request, deliverMSG, NNr, ToClient}} ->
      deliver_nachricht_handler(PID, NNr, ToClient, DeliveryQueue),
      receive_loop(HoldbackQueue, DeliveryQueue);

    {PID, {request, dellHBQ}} -> delete_hbq_handler(PID, DeliveryQueue)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

%Bestaetigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
init_hbq_handler(PID) ->
  PID ! {reply, ok}.

push_hbq_handler(PID, [NNr, MessageLST, TSClientout], HBQ, DLQ) ->
  % 1) Pruefen ob die geschickte Nachrichtennummer bereits auslieferbar ist
  logge_status(io_lib:format("push_hbq_handler PID: ~p, MsgNr: ~p", [PID, NNr])),
  TShbqin = erlang:timestamp(),
  MessageWithAddedTS = [NNr, MessageLST, TSClientout, TShbqin],
  CanBeDelivered = is_in_order(MessageWithAddedTS, DLQ),
  logge_status(io_lib:format("Kann Nachricht gesendet werden? ~p", [CanBeDelivered])),
  % 2) falls ja --> mittels push2DLQ ausliefern und Reihenfolge der bereits gespeicherten Nachrichten pruefen; ggf weitere Nachrichten an die DLQ schicken
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(MessageWithAddedTS, DLQ, ?DLQ_LOG_DATEI), 
      {NewHBQ, ResultDLQ} = pruefe_naechste_nachricht_und_pushe(HBQ, NewDLQ),
      Result = {NewHBQ, ResultDLQ};
  % 3) falls nein --> Nachricht in die HBQ einsortieren (den Nummern nach aufsteigend)
    true ->
      NewHBQ = in_hbq_einfuegen(MessageWithAddedTS, HBQ),
      % Limit abpruefen (2/3el der DLQ) und ggf Luecke schließen
      NewDLQ = pruefe_limit_und_fuelle_spalte(NewHBQ, DLQ, ?DLQLIMIT),
      {ResultHBQ,ResultDLQ} = pruefe_naechste_nachricht_und_pushe(NewHBQ,NewDLQ),
      Result = {ResultHBQ,ResultDLQ}
  end,
  PID ! {reply, ok},
  Result.

%Handler fuer den deliverMSG Befehl. Dieser wird an die dlq delegiert, anschließend wird die Nummer der versendeten Nachricht an den
%Server zurueckgegeben
deliver_nachricht_handler(PID, NNr, ToClient, DLQ) ->
  SentMsgNum = dlq:deliverMSG(NNr,ToClient,DLQ, ?DLQ_LOG_DATEI),
  logge_status(io_lib:format("Number of sent Message: ~p", [SentMsgNum])),
  PID ! {reply,SentMsgNum}.

%Handler fuer den delete Befehl, loescht auch die DLQ
delete_hbq_handler(PID, DLQ) ->
  AtomAnswer = dlq:delDLQ(DLQ),
  if 
    (AtomAnswer == ok) ->
      logge_status("DLQ wurde erfolgreich geloescht");
    true -> 
      logge_status("ERR: DLQ wurde NICHT geloescht")
  end,
  PID ! {reply, ok},
  exit(self(),kill).

%------------------------------------------------------------------------------------------------------
%																					>>HILFSMETHODEN<<
%------------------------------------------------------------------------------------------------------

%Hilfsmethode um zu pruefen, ob die DLQ diese Nachricht erwartet
is_in_order([NNr | _], DLQ) ->
  ExpectedNNr = dlq:expectedNr(DLQ),
  ExpectedNNr == NNr.

%Rekursive Hilfsmethode, welche nach und nach weitere Nachrichten an die DLQ schickt, falls moeglich
pruefe_naechste_nachricht_und_pushe([], DLQ) -> {[], DLQ};
pruefe_naechste_nachricht_und_pushe([HBQHead | HBQTail], DLQ) ->
 %logge_status(io_lib:format("Aktueller Head: ~p", [HBQHead])),
  CanBeDelivered = is_in_order(HBQHead, DLQ),
  logge_status(io_lib:format("Message CanBeDelivered? ~p", [CanBeDelivered])),
  case CanBeDelivered of
    true ->
      NewDLQ = dlq:push2DLQ(HBQHead, DLQ, ?DLQ_LOG_DATEI),
      pruefe_naechste_nachricht_und_pushe(HBQTail, NewDLQ);
    false -> 
      {[HBQHead | HBQTail], DLQ}
  end.

in_hbq_einfuegen(Message, []) ->
  logge_status("HBQ ist leer, Element wird einfach eingefuegt"),
  [Message];
in_hbq_einfuegen(Message, HBQ) ->
  logge_status("HBQ ist nicht leer"),
  NewHBQ = in_hbq_einfuegen_([], Message, HBQ),
  NewHBQ.

%Prueft pro Rekursionsschritt, ob das forderste Element der HBQ eine kleinere NNR hat als das einzufuegende Element.
%Wenn dem nicht so ist, wird das erste Element der HBQ in den Akku geschrieben und es erfolgt ein weiterer Rekursionsaufruf
%Wenn dem so ist, wird das Neue Element an den Akku gehaengt, dieser vor den Rest der HBQ gehaengt und das ganze zurueckgegeben ausgegeben.
in_hbq_einfuegen_(Akku, Message, []) ->
  logge_status("ist ganz durchgelaufen, element wird ganz hinten angehaengt"),
  NewHBQ = lists:append(Akku, [Message]),
  NewHBQ;
in_hbq_einfuegen_(Akku, [NNr | MessageRest], [[HBQHeadNNr | HBQHeadRest] | HBQTail]) ->
  IsSmaller = NNr < HBQHeadNNr,
  if (IsSmaller) ->
    %Hier war der Fehler :)
    NewFirstPartOfHBQ = lists:append(Akku, [[NNr | MessageRest]]),
    RestOfHBQ = [[HBQHeadNNr | HBQHeadRest] | HBQTail],
    NewHBQ = lists:append(NewFirstPartOfHBQ, RestOfHBQ);
    true ->
      NewAkku = lists:append(Akku, [[HBQHeadNNr | HBQHeadRest]]),
      NewHBQ = in_hbq_einfuegen_(NewAkku, [NNr | MessageRest], HBQTail)
  end,
  NewHBQ.

%Hilfsmethode zum ueberpruefen der Einhaltung des 2/3el Ansatzes
pruefe_limit_und_fuelle_spalte(HBQ, DLQ, DLQLimit) ->
  GapLimit = DLQLimit / 3 * 2,
  HBQSize = length(HBQ),
  if
    (HBQSize > GapLimit) ->
      logge_status("2/3 Regel erfuellt"),
      %Ist das Limit ueberschritten, so wird die erste Luecke geschlossen und an die DLQ weitergegeben
      NewDLQ = suche_und_fuelle_spalte(HBQ, DLQ),
      NewDLQ;
    true ->
      %Ist das Limit nicht ueberschritten, bleiben die beiden Queues unveraendert
      logge_status("2/3 Regel nicht erfuellt"),
      DLQ
  end.

% Hilfsmethode die herausfindet wo genau die Luecke ist und die Luecke schließt.
suche_und_fuelle_spalte(HBQ, DLQ) ->
  % Wo ist die Luecke?
  {SpaltStartNNr, SpaltEndeNNr} = suche_spalte(HBQ, DLQ),
  % Erstelle Fehlernachricht und logge
  GapMessageList = erstelle_spalt_nachricht(SpaltStartNNr, SpaltEndeNNr),
  % Schließe diese Luecke
  NewDLQ = dlq:push2DLQ(GapMessageList, DLQ, ?LOG_DATEI_NAME),
  NewDLQ.

% Hilfsmethode die herausfindet wo genau die Luecke ist.
suche_spalte([[AktuelleNNr | _AktuelleNachrichtRest] | _HBQRest],DLQ) ->
  ErwarteteNNr = dlq:expectedNr(DLQ),
  logge_status(io_lib:format("Gaprange: ~p-~p",[ErwarteteNNr, AktuelleNNr - 1])),
  {ErwarteteNNr, AktuelleNNr - 1}.

% Erstellt eine Mock Nachricht um die Luecke schließen zu koennen.
erstelle_spalt_nachricht(SpaltStartNNr,SpaltEndeNNr) ->
  TS = erlang:timestamp(),
  [SpaltEndeNNr, lists:flatten(io_lib:format("Error Nachricht zum Luecke von ~p bis ~p zu fuellen", [SpaltStartNNr, SpaltEndeNNr])), TS, TS].

%------------------------------------------------------------------------------------------------------
%																					>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

% Holt aus der Configdatei (server.cfg) den benoetigten Wert fuer den eingegebenen Key.
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
