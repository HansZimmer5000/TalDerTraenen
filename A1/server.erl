
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
-define(TIMEOUT, 5).

%------------------------------------------------------------------------------------------------------
%											>>START / INIT<<
%------------------------------------------------------------------------------------------------------
% Hierueber wird der Server gestartet und die HBQ und CMEM initialisiert.
start() -> 
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?CMEM_LOG_DATEI_NAME),
    initHBQ(),
    ServerPid = spawn(fun() -> receive_loop(CMEM, 1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.

% Initalisiert die HBQ und wartet bis diese Antwortet.
initHBQ() ->
    net_adm:ping(?HBQNODE),
    ?HBQ ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> logge_status("HBQ initalisiert")
    end.
    
%------------------------------------------------------------------------------------------------------
%												>>LOOP<<
%------------------------------------------------------------------------------------------------------
% Hierrueber werden die im Entwurf beschriebenen Schnittstellen angeboten.
receive_loop(CMEM, NextNNR) ->
    receive
        {AbsenderPid, getmessages} ->   NeueCMEM = getmessages_abfertigen(?HBQ, CMEM, AbsenderPid),
                                        receive_loop(NeueCMEM, NextNNR);
        {dropmessage, Nachricht} ->     dropmessage_abfertigen(?HBQ, Nachricht),
                                        receive_loop(CMEM, NextNNR);
        {AbsenderPid, getmsgid} ->  	NeueNextNNR = getmsgid_abfertigen(AbsenderPid, NextNNR),
										receive_loop(CMEM, NeueNextNNR);
        UnbekanntesKommando ->  		logge_status(io_lib:format("Bekam unbekanntes Kommando ~p", [UnbekanntesKommando])),
										receive_loop(CMEM, NextNNR)
        
        after timer:seconds(?LATENZ_SEK) -> runterfahren(CMEM)
    end.


%------------------------------------------------------------------------------------------------------
%                           >>EIGENTLICHE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
% Diese Funktion wickelt die Leseanfrage des Clients ab.
%  Wir im Entwurd beschrieben wird die naechste NNR aus der CMEM gelesen, diese an die HBQ gesendet und hirnach die CMEM aktualisiert.
getmessages_abfertigen(HBQPid, CMEM, LeserPid) -> 
    ZuSendendeNNr = hole_naechste_nnr_fur_leser(CMEM, LeserPid),
    GesendeteNNr = sendeNNr(HBQPid, ZuSendendeNNr, LeserPid),
    logge_status(io_lib:format("Nachricht mit Nummer ~p (Angefordert: ~p) an ~p gesendet", [GesendeteNNr, ZuSendendeNNr, LeserPid])),
    NeueCMEM = update_gesendete_nnr_fur_leser(CMEM, LeserPid, GesendeteNNr),
    NeueCMEM.

% Diese Funktion hollt aus der CMEM die naechste NNR für den jeweiligen Client. 
hole_naechste_nnr_fur_leser(CMEM, LeserPid) ->
    cmem:getClientNNr(CMEM, LeserPid).

% Diese Funktionen übergibt der CMEM die letzte gesendete NNR zum jeweiigen Client um diese zu updaten.
update_gesendete_nnr_fur_leser(CMEM, LeserPid, LetzteGesendeteNNr) ->
    NeueCMEM = cmem:updateClient(CMEM, LeserPid, LetzteGesendeteNNr, ?CMEM_LOG_DATEI_NAME),
    NeueCMEM.

% Wir im Entwurf beschrieben beauftragt der Server die HBQ, die NNR an den jeweiligen Client zu senden und gibt bei erfolg die gesendeteNNR zurück.
sendeNNr(HBQPid, ZuSendendeNNr, LeserPid) ->
    HBQPid ! {self(), {request, deliverMSG, ZuSendendeNNr, LeserPid}},
    receive
        {reply, GesendeteNNr} -> 
			GesendeteNNr
		after timer:seconds(?TIMEOUT) -> 
            logge_status(io_lib:format("Die HBQ konnte nicht beauftragt werden, die Nachricht mit der der Nummer ~p an den Client zu schicken.", [ZuSendendeNNr]))
	end.

% Wie im Entwurd beschrieben wird die Nachricht vom Client an die HBQ uebergeben, das Ergebnis wird geloggt.
dropmessage_abfertigen(HBQPid, Nachricht) ->
    HBQPid ! {self(), {request, pushHBQ, Nachricht}},
    [NNr | _Rest] = Nachricht,
    receive
        {reply, ok} ->
            logge_status(io_lib:format("Nachricht mit Nummer ~p wurde an HBQ geschickt", [NNr]))
        after timer:seconds(?TIMEOUT) -> 
            logge_status(io_lib:format("Nachricht mit Nummer ~p wurde NICHT erfolgreich an HBQ geschickt", [NNr]))
    end.

% Hierüber wird eine eindeutige NNR an den Client verschickt.
getmsgid_abfertigen(AbsenderPid, LetzteNNR) -> 
    AbsenderPid ! {nid, LetzteNNR},
    logge_status(io_lib:format("NNr ~p an ~p gesendet", [LetzteNNR, AbsenderPid])),
    LetzteNNR + 1.


%Hierrüber werden die HBQ und CMEM gelöscht.
runterfahren(CMEM) ->
    logge_status("Server wird heruntergefahren"),
    ?HBQ ! {self(), {request, dellHBQ}},
    receive
        {reply, ok} -> 
			logge_status("HBQ erfolgreich heruntergefahren")
        after timer:seconds(?TIMEOUT) -> 
			logge_status("HBQ nicht erfolgreich heruntergefahren")
    end,
    ok = cmem:delCMEM(CMEM),
    true = unregister(?SERVERNAME),
    logge_status("-done").

%------------------------------------------------------------------------------------------------------
%										>>GENERELLE FUNKTIONEN<<
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

