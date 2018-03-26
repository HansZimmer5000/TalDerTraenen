-module(hbq).

% API
-export([
    startHBQ/0,

    start/0,
    wait_for_init/0,
    init_hbq/1,

    receive_loop/2,

    push_hbq/4,
    in_hbq_einfuegen/2,
    deliver_nachricht/4,
    delete_hbq/2,

    is_in_order/2,
    pruefe_naechste_nachricht_und_pushe/2,
    pruefe_limit_und_fuelle_spalte/3,
    finde_und_fuelle_spalte/2,
    finde_spalte/2,
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
% Code vom Professor erwartet folgende Funktion
startHBQ() ->
  start().

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
      init_hbq(PID), 
      receive_loop([], DLQ)
  end.

receive_loop(HoldbackQueue, DeliveryQueue) ->
  io:fwrite("receive_loop"),
  receive
    {PID, {request, pushHBQ, MessageAsList}} ->
      {NewHBQ, NewDLQ} = push_hbq(PID, MessageAsList, HoldbackQueue, DeliveryQueue),
      receive_loop(NewHBQ, NewDLQ);

    {PID, {request, deliverMSG, NNr, ToClient}} ->
      deliver_nachricht(PID, NNr, ToClient, DeliveryQueue),
      receive_loop(HoldbackQueue, DeliveryQueue);

    {PID, {request, dellHBQ}} -> delete_hbq(PID, DeliveryQueue)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

%Bestaetigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
init_hbq(PID) ->
  PID ! {reply, ok}.

push_hbq(PID, [NNr, MessageLST, TSClientout], HBQ, DLQ) ->
  logge_status(io_lib:format("push_hbq PID: ~p, MsgNr: ~p", [PID, NNr])),
  TShbqin = erlang:timestamp(),
  MessageWithAddedTS = [NNr, MessageLST, TSClientout, TShbqin],
  CanBeDelivered = is_in_order(MessageWithAddedTS, DLQ),
  logge_status(io_lib:format("Kann Nachricht gesendet werden? ~p", [CanBeDelivered])),
  if 
    CanBeDelivered ->
      NewDLQ = dlq:push2DLQ(MessageWithAddedTS, DLQ, ?DLQ_LOG_DATEI), 
      {NewHBQ, ResultDLQ} = pruefe_naechste_nachricht_und_pushe(HBQ, NewDLQ),
      Result = {NewHBQ, ResultDLQ};
    true ->
      NewHBQ = in_hbq_einfuegen(MessageWithAddedTS, HBQ),
      NewDLQ = pruefe_limit_und_fuelle_spalte(NewHBQ, DLQ, ?DLQLIMIT),
      {ResultHBQ,ResultDLQ} = pruefe_naechste_nachricht_und_pushe(NewHBQ,NewDLQ),
      Result = {ResultHBQ,ResultDLQ}
  end,
  PID ! {reply, ok},
  Result.

%Handler fuer den deliverMSG Befehl. Dieser wird an die dlq delegiert, anschließend wird die Nummer der versendeten Nachricht an den
%Server zurueckgegeben
deliver_nachricht(PID, NNr, ToClient, DLQ) ->
  SentMsgNum = dlq:deliverMSG(NNr,ToClient,DLQ, ?DLQ_LOG_DATEI),
  logge_status(io_lib:format("Number of sent Message: ~p", [SentMsgNum])),
  PID ! {reply,SentMsgNum}.

%Handler fuer den delete Befehl, loescht auch die DLQ
delete_hbq(PID, DLQ) ->
  AtomAnswer = dlq:delDLQ(DLQ),
  if 
    (AtomAnswer == ok) ->
      logge_status("DLQ wurde erfolgreich geloescht");
    true -> 
      logge_status("ERR: DLQ wurde NICHT geloescht")
  end,
  PID ! {reply, ok},
  true = unregister(?HBQNAME).

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
      NewDLQ = finde_und_fuelle_spalte(HBQ, DLQ),
      NewDLQ;
    true ->
      logge_status("2/3 Regel nicht erfuellt"),
      DLQ
  end.

% Hilfsmethode die herausfindet wo genau die Luecke ist und die Luecke schließt.
finde_und_fuelle_spalte(HBQ, DLQ) ->
  {SpaltStartNNr, SpaltEndeNNr} = finde_spalte(HBQ, DLQ),
  GapMessageList = erstelle_spalt_nachricht(SpaltStartNNr, SpaltEndeNNr),
  NewDLQ = dlq:push2DLQ(GapMessageList, DLQ, ?LOG_DATEI_NAME),
  NewDLQ.

% Hilfsmethode die herausfindet wo genau die Luecke ist.
finde_spalte([[AktuelleNNr | _AktuelleNachrichtRest] | _HBQRest], DLQ) ->
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

extractValueFromConfig(Key) ->
  {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
  {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
  Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).
