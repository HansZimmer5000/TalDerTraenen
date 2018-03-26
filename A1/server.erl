
-module(server).

% API
-export([start/0,

        getmessages_abfertigen/3,
        hole_naechste_nnr_fur_leser/2,
        update_gesendete_nnr_fur_leser/3,
        sendeNNr/3,

        dropmessage_abfertigen/2,

        getmsgid_abfertigen/2
        ]).

% KONSTANTEN
-define(CONFIG_FILENAME, "server.cfg").
-define(LOG_DATEI_NAME, "server.log").
-define(SERVERNAME, hole_wert_aus_config_mit_key(servername)).
-define(LATENZ_SEK, hole_wert_aus_config_mit_key(latency)).
-define(CMEM_LOG_DATEI_NAME, "cmem.log").
-define(ERINNERUNGS_ZEIT_SEK, hole_wert_aus_config_mit_key(clientlifetime)).
-define(HBQNAME, hole_wert_aus_config_mit_key(hbqname)).
-define(HBQNODE, hole_wert_aus_config_mit_key(hbqnode)).
-define(HBQ, {?HBQNAME, ?HBQNODE}).

%------------------------------------------------------------------------------------------------------
%																	>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start() -> 
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?CMEM_LOG_DATEI_NAME),
    initHBQ(),
    ServerPid = spawn(fun() -> receive_loop(CMEM, 1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.

initHBQ() ->
    net_adm:ping(?HBQNODE),
    ?HBQ ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> logge_status("HBQ initalisiert")
    end.
    
%------------------------------------------------------------------------------------------------------
%																	>>LOOP<<
%------------------------------------------------------------------------------------------------------
receive_loop(CMEM, NextNNR) ->
    receive
        {AbsenderPid, getmessages} ->   NeueCMEM = getmessages_abfertigen(?HBQ, CMEM, AbsenderPid),
                                        receive_loop(NeueCMEM, NextNNR);
        {dropmessage, Nachricht} ->     dropmessage_abfertigen(?HBQ, Nachricht),
                                        receive_loop(CMEM, NextNNR);
        {AbsenderPid, getmsgid} ->  NeueNextNNR = getmsgid_abfertigen(AbsenderPid, NextNNR),
                                    receive_loop(CMEM, NeueNextNNR)
        after timer:seconds(?LATENZ_SEK) -> runterfahren(CMEM)
    end.


%------------------------------------------------------------------------------------------------------
%																	>>EIGENTLICHE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
getmessages_abfertigen(HBQPid, CMEM, LeserPid) -> 
    ZuSendendeNNr = hole_naechste_nnr_fur_leser(CMEM, LeserPid),
    GesendeteNNr = sendeNNr(HBQPid, ZuSendendeNNr, LeserPid),
    logge_status(io_lib:format("Nachricht mit Nummer ~p (Angefordert: ~p) an ~p gesendet", [ZuSendendeNNr, GesendeteNNr, LeserPid])),
    NeueCMEM = update_gesendete_nnr_fur_leser(CMEM, LeserPid, GesendeteNNr),
    NeueCMEM.

hole_naechste_nnr_fur_leser(CMEM, LeserPid) ->
    cmem:getClientNNr(CMEM, LeserPid).

update_gesendete_nnr_fur_leser(CMEM, LeserPid, LetzteGesendeteNNr) ->
    NeueCMEM = cmem:updateClient(CMEM, LeserPid, LetzteGesendeteNNr, ?CMEM_LOG_DATEI_NAME),
    NeueCMEM.

sendeNNr(HBQPid, ZuSendendeNNr, LeserPid) ->
    HBQPid ! {self(), {request, deliverMSG, ZuSendendeNNr, LeserPid}},
    receive
        {reply, GesendeteNNr} -> GesendeteNNr
    end.


dropmessage_abfertigen(HBQPid, Nachricht) ->
    HBQPid ! {self(), {request, pushHBQ, Nachricht}},
    [NNr | _Rest] = Nachricht,
    receive
        {reply, ok} ->
            logge_status(io_lib:format("Nachricht mit Nummer ~p wurde an HBQ geschickt", [NNr]))
        after timer:seconds(5) -> 
            logge_status(io_lib:format("Nachricht mit Nummer ~p wurde NICHT erfolgreich an HBQ geschickt", [NNr]))
    end.


getmsgid_abfertigen(AbsenderPid, LetzteNNR) -> 
    AbsenderPid ! {nid, LetzteNNR},
    logge_status(io_lib:format("NNr ~p an ~p gesendet", [LetzteNNR, AbsenderPid])),
    LetzteNNR + 1.



runterfahren(CMEM) ->
    logge_status("Server wird heruntergefahren"),
    ?HBQ ! {self(), {request, dellHBQ}},
    receive
        {reply, ok} -> logge_status("HBQ erfolgreich heruntergefahren")
        after timer:seconds(5) -> logge_status("HBQ nicht erfolgreich heruntergefahren")
    end,
    ok = cmem:delCMEM(CMEM),
    true = unregister(?SERVERNAME),
    logge_status("-done").

%------------------------------------------------------------------------------------------------------
%																	>>GENERELLE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).

