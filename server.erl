
-module(server).

% API
-export([start/0,
        getmessages_abfertigen/2,
        dropmessage_abfertigen/1,
        getmsgid_abfertigen/2]).

% CONSTANTS
-define(CONFIG_FILENAME, "server.cfg").
-define(LOG_DATEI_NAME, "server.log").
-define(SERVERNAME, hohle_wert_aus_config_mit_key(servername)).
-define(LATENZ_SEK, hohle_wert_aus_config_mit_key(latenzSek)).
-define(CMEM_LOG_DATEI_NAME, "cmem.log").
-define(ERINNERUNGS_ZEIT_SEK, 3).

start() -> 
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?CMEM_LOG_DATEI_NAME),
    %HBQPID = b,
    ServerPid = spawn(fun() -> receive_loop(CMEM, 1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.


receive_loop(CMEM, NextNNR) ->
    logge_status("receive_loop"),
    {ok,ServerTimer} = timer:send_after(?LATENZ_SEK, self(), {request,killAll}),
    receive
        {AbsenderPID, getmessages} ->   timer:cancel(ServerTimer),
                                        NeueCMEM = getmessages_abfertigen(CMEM, AbsenderPID),
                                        receive_loop(NeueCMEM, NextNNR);
        {dropmessage, Nachricht} ->     timer:cancel(ServerTimer),
                                        dropmessage_abfertigen(Nachricht),
                                        receive_loop(CMEM, NextNNR);
        {AbsenderPID, getmsgid} ->  timer:cancel(ServerTimer),
                                    NeueNextNNR = getmsgid_abfertigen(AbsenderPID, NextNNR),
                                    receive_loop(CMEM, NeueNextNNR);
        {request, killAll} -> runterfahren()
    end.


getmessages_abfertigen(_CMEM, EmpfaengerPID) ->  
    logge_status("Got getmessages"),  
    TS = erlang:timestamp(),
    Nachricht = [1, "Text", TS, TS, TS, TS],
    TerminatedFlag = rand:uniform() > 0.5,
    EmpfaengerPID ! {reply, Nachricht, TerminatedFlag}.


dropmessage_abfertigen(Nachricht) ->
    logge_status("Got dropmessage"),
    logge_nachricht_status(Nachricht, "erhalten").


getmsgid_abfertigen(AbsenderPID, LetzteNNR) -> 
    logge_status("Got getmsgid"),  
    AbsenderPID ! {nid, LetzteNNR},
    LetzteNNR + 1.

runterfahren() ->
    logge_status("Server wird heruntergefahren").



hohle_wert_aus_config_mit_key(Key) ->
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
