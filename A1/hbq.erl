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

    wird_erwartet/2,
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
% Alternative zu start/0
startHBQ() ->
  start().

start() ->
  logge_status("HBQ wird gestartet"),
  HBQPID = spawn(fun() -> wait_for_init() end),
  register(?HBQNAME, HBQPID),
  HBQPID.

% Wartet auf Init vom Server
wait_for_init() ->
  receive
    {PID, {request, initHBQ}} ->
      DLQ = init_hbq(PID), 
      logge_status("HBQ initiiert und running"),
      receive_loop([], DLQ);
    {PID, {request, dellHBQ}} -> 
      delete_hbq(PID)
  end.

receive_loop(HBQ, DLQ) ->
  receive
    {PID, {request, pushHBQ, NachrichtAsList}} ->
      {NeueHBQ, NeueDLQ} = push_hbq(PID, NachrichtAsList, HBQ, DLQ),
      receive_loop(NeueHBQ, NeueDLQ);

    {PID, {request, deliverMSG, NNr, ToClient}} ->
      deliver_nachricht(PID, NNr, ToClient, DLQ),
      receive_loop(HBQ, DLQ);

    {PID, {request, dellHBQ}} -> 
      delete_hbq(PID, DLQ);

    UnbekanntesKommando ->
      logge_status(io_lib:format("Bekam unbekanntes Kommando ~p", [UnbekanntesKommando])),
      receive_loop(HBQ, DLQ)
  end.

%------------------------------------------------------------------------------------------------------
%																	>>SCHNITTSTELLEN<<
%------------------------------------------------------------------------------------------------------

%Bestaetigt dem aufrufenden Prozess (in diesem Fall dem Server) die Initialisierung
init_hbq(PID) ->
  DLQ = dlq:initDLQ(?DLQLIMIT, ?DLQ_LOG_DATEI),
  PID ! {reply, ok},
  DLQ.

push_hbq(PID, Nachricht, HBQ, DLQ) ->
  NachrichtMitTs = fuege_hbqin_ts_hinzu(Nachricht),

  HBQDLQTupel = pruefe_und_sende_nachricht(NachrichtMitTs, HBQ, DLQ),

  PID ! {reply, ok},
  HBQDLQTupel.

fuege_hbqin_ts_hinzu([NNr, Text, TSClientout]) ->
  TShbqin = erlang:timestamp(),
  [NNr, Text, TSClientout, TShbqin].

pruefe_und_sende_nachricht(Nachricht, HBQ, DLQ) ->
  KannDirektAnDLQ = wird_erwartet(Nachricht, DLQ),
  case KannDirektAnDLQ of 
    true ->
      logge_nachricht_status("an DLQ gesendet", Nachricht),
      NeueHBQ = HBQ,
      NeueDLQ = dlq:push2DLQ(Nachricht, DLQ, ?DLQ_LOG_DATEI);
    false ->
      logge_nachricht_status("in HBQ sortiert", Nachricht),
      NeueHBQ = in_hbq_einfuegen(Nachricht, HBQ),
      NeueDLQ = pruefe_limit_und_fuelle_spalte(NeueHBQ, DLQ, ?DLQLIMIT)
  end,
  HBQDLQTupel = pruefe_naechste_nachricht_und_pushe(NeueHBQ,NeueDLQ),
  HBQDLQTupel.

%Hilfsmethode um zu pruefen, ob die DLQ diese Nachricht erwartet
wird_erwartet([NNr | _Rest], DLQ) ->
  ExpectedNNr = dlq:expectedNr(DLQ),
  ExpectedNNr == NNr.

%Rekursive Hilfsmethode, welche nach und nach weitere Nachrichten an die DLQ schickt, falls moeglich
pruefe_naechste_nachricht_und_pushe([], DLQ) -> {[], DLQ};
pruefe_naechste_nachricht_und_pushe([HBQHead | HBQRest], DLQ) ->
  KannGesendetWerden = wird_erwartet(HBQHead, DLQ),
  case KannGesendetWerden of
    true ->
      logge_nachricht_status("in DLQ verschoben", HBQHead),
      NeueDLQ = dlq:push2DLQ(HBQHead, DLQ, ?DLQ_LOG_DATEI),
      pruefe_naechste_nachricht_und_pushe(HBQRest, NeueDLQ);
    false -> 
      {[HBQHead | HBQRest], DLQ}
  end.

%Hilfsmethode zum ueberpruefen der Einhaltung des 2/3el Ansatzes
pruefe_limit_und_fuelle_spalte(HBQ, DLQ, DLQLimit) ->
  GapLimit = DLQLimit / 3 * 2,
  HBQSize = length(HBQ),
  case HBQSize > GapLimit of
    true ->
      logge_status("2/3 Regel erfuellt"),
      NeueDLQ = finde_und_fuelle_spalte(HBQ, DLQ),
      NeueDLQ;
    false ->
      logge_status("2/3 Regel nicht erfuellt"),
      DLQ
  end.

% Fuegt eine Nachricht korrekt (Sortierung) in die HBQ ein.
in_hbq_einfuegen(Nachricht, HBQ) ->
  NeueHBQ = in_hbq_einfuegen_([], Nachricht, HBQ),
  NeueHBQ.

%Prueft pro Rekursionsschritt, ob das vorderste Element der HBQ eine kleinere NNR hat als das einzufuegende Element.
%Wenn dem nicht so ist, wird das erste Element der HBQ in den Akku geschrieben und es erfolgt ein weiterer Rekursionsaufruf
%Wenn dem so ist, wird das Neue Element an den Akku gehaengt, dieser vor den Rest der HBQ gehaengt und das ganze zurueckgegeben ausgegeben.
in_hbq_einfuegen_(Akku, Nachricht, []) ->
  logge_nachricht_status("wurde ganz hinten an HBQ gehaengt", Nachricht),
  NeueHBQ = Akku ++ [Nachricht],
  NeueHBQ;
in_hbq_einfuegen_(Akku, [NNr | NachrichtRest], [[HBQKopfNNr | HBQKopfRest] | HBQRest]) ->
  case NNr < HBQKopfNNr of 
    true ->
      VordererHBQTeil = Akku ++ [[NNr | NachrichtRest]],
      NeuerHBQRest = [[HBQKopfNNr | HBQKopfRest] | HBQRest],
      NeueHBQ = VordererHBQTeil ++ NeuerHBQRest;
    false ->
      NeuerAkku = Akku ++ [[HBQKopfNNr | HBQKopfRest]],
      NeueHBQ = in_hbq_einfuegen_(NeuerAkku, [NNr | NachrichtRest], HBQRest)
  end,
  NeueHBQ.

%Handler fuer den deliverMSG Befehl. Dieser wird an die dlq delegiert, anschließend wird die Nummer der versendeten Nachricht an den
%Server zurueckgegeben
deliver_nachricht(PID, NNr, ToClient, DLQ) ->
  GesendeteNNr = dlq:deliverMSG(NNr,ToClient,DLQ, ?DLQ_LOG_DATEI),
  PID ! {reply, GesendeteNNr}.

%Handler fuer den delete Befehl, loescht auch die DLQ
delete_hbq(PID) ->
  unregisterHBQ(?HBQNAME),
  PID ! {reply, ok}.
delete_hbq(PID, DLQ) ->
  delete_dlq(DLQ),
  unregisterHBQ(?HBQNAME),
  PID ! {reply, ok}.

delete_dlq(DLQ) ->
  case dlq:delDLQ(DLQ) of 
    ok ->
      logge_status("DLQ wurde erfolgreich geloescht");
    _Any -> 
      logge_status("ERR: DLQ wurde NICHT geloescht")
  end.

unregisterHBQ(HBQName) ->
  case unregister(HBQName) of
    true ->
      logge_status("HBQ wurde erfolgreich unregistered");
    _Any ->
      logge_status("ERR: HBQ wurde NICHT unregistered")
  end.


% Hilfsmethode die herausfindet wo genau die Luecke ist und die Luecke schließt.
finde_und_fuelle_spalte(HBQ, DLQ) ->
  {SpaltStartNNr, SpaltEndeNNr} = finde_spalte(HBQ, DLQ),
  GapNachrichtList = erstelle_spalt_nachricht(SpaltStartNNr, SpaltEndeNNr),
  NeueDLQ = dlq:push2DLQ(GapNachrichtList, DLQ, ?LOG_DATEI_NAME),
  NeueDLQ.

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

logge_nachricht_status(Inhalt, Nachricht) ->
    [NNr | _Rest] = Nachricht,
    logge_status(io_lib:format("Nachricht mit NNr ~p: ~s", [NNr, Inhalt])).
    
