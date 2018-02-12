
-module(server).

% API
-export([start/0,

        getmessages_abfertigen/2,
        hole_naechste_nnr_fur_leser/2,
        update_gesendete_nnr_fur_leser/3,
        sendeNNr/2,

        dropmessage_abfertigen/1,

        getmsgid_abfertigen/2
        ]).

% CONSTANTS
-define(CONFIG_FILENAME, "server.cfg").
-define(LOG_DATEI_NAME, "server.log").
-define(SERVERNAME, hole_wert_aus_config_mit_key(servername)).
-define(LATENZ_SEK, hole_wert_aus_config_mit_key(latenzSek)).
-define(CMEM_LOG_DATEI_NAME, "cmem.log").
-define(ERINNERUNGS_ZEIT_SEK, 3).
-define(HBQNAME, hole_wert_aus_config_mit_key(hbqname)).
-define(HBQNODE, hole_wert_aus_config_mit_key(hbqnode)).
-define(HBQ, {?HBQNAME, ?HBQNODE}).

start() -> 
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?CMEM_LOG_DATEI_NAME),
    %HBQPid = b,
    ServerPid = spawn(fun() -> receive_loop(CMEM, 1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.


receive_loop(CMEM, NextNNR) ->
    logge_status("receive_loop"),
    {ok,ServerTimer} = timer:send_after(?LATENZ_SEK, self(), {request,killAll}),
    receive
        {AbsenderPid, getmessages} ->   logge_status("Got getmessages"),
                                        timer:cancel(ServerTimer),
                                        NeueCMEM = getmessages_abfertigen(CMEM, AbsenderPid),
                                        receive_loop(NeueCMEM, NextNNR);
        {dropmessage, Nachricht} ->     logge_status("Got dropmessage"),
                                        timer:cancel(ServerTimer),
                                        dropmessage_abfertigen(Nachricht),
                                        receive_loop(CMEM, NextNNR);
        {AbsenderPid, getmsgid} ->  logge_status("Got getmsgid"),
                                    timer:cancel(ServerTimer),
                                    NeueNextNNR = getmsgid_abfertigen(AbsenderPid, NextNNR),
                                    receive_loop(CMEM, NeueNextNNR);
        {request, killAll} -> runterfahren()
    end.


getmessages_abfertigen(CMEM, LeserPid) -> 
    ZuSendendeNNr = hole_naechste_nnr_fur_leser(CMEM, LeserPid),
    sendeNNr(ZuSendendeNNr, LeserPid),
    NeueCMEM = update_gesendete_nnr_fur_leser(CMEM, LeserPid, ZuSendendeNNr),
    NeueCMEM.

hole_naechste_nnr_fur_leser(CMEM, LeserPid) ->
    cmem:getClientNNr(CMEM, LeserPid).
    
update_gesendete_nnr_fur_leser(CMEM, LeserPid, LetzteGesendeteNNr) ->
    cmem:updateClient(CMEM, LeserPid, LetzteGesendeteNNr, ?CMEM_LOG_DATEI_NAME).

sendeNNr(ZuSendendeNNr, LeserPid) ->
    TS = erlang:timestamp(),
    Nachricht = [ZuSendendeNNr, "Text", TS, TS, TS, TS],
    TerminatedFlag = rand:uniform() > 0.5,
    LeserPid ! {reply, Nachricht, TerminatedFlag}.


dropmessage_abfertigen(Nachricht) ->
    logge_nachricht_status(Nachricht, "erhalten").


getmsgid_abfertigen(AbsenderPid, LetzteNNR) -> 
    AbsenderPid ! {nid, LetzteNNR},
    LetzteNNR + 1.



runterfahren() ->
    logge_status("Server wird heruntergefahren").



hole_wert_aus_config_mit_key(Key) ->
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
