
-module(server).

% API
-export([start/0,
        getmessages_abfertigen/1,
        dropmessage_abfertigen/1,
        getmsgid_abfertigen/2]).

% CONSTANTS
-define(CONFIG_FILENAME, "server.cfg").
-define(LOG_DATEI_NAME, "server.log").
-define(SERVERNAME, hohle_wert_aus_config_mit_key(servername)).

start() -> 
    %CMEM = a.
    %HBQPID = b.
    ServerPid = spawn(fun() -> receive_loop(1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.


receive_loop(NextNNR) ->
    logge_status("receive_loop"),
    receive
        {AbsenderPID, getmessages} ->   getmessages_abfertigen(AbsenderPID),
                                        receive_loop(NextNNR);
        {dropmessage, Nachricht} ->     dropmessage_abfertigen(Nachricht),
                                        receive_loop(NextNNR);
        {AbsenderPID, getmsgid} ->  NeueNextNNR = getmsgid_abfertigen(AbsenderPID, NextNNR),
                                    receive_loop(NeueNextNNR)
    end.


getmessages_abfertigen(EmpfaengerPID) ->  
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
