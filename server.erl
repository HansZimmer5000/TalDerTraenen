
-module(server).

% API
-export([start/0,
        getmessages_abfertigen/1,
        dropmessage_abfertigen/1,
        getmsgid_abfertigen/2]).

% CONSTANTS
-define(CONFIG_FILENAME, "server.cfg").
-define(SERVERNAME, hohle_wert_aus_config_mit_key(servername)).

start() -> 
    %CMEM = a.
    %HBQ = b.
    ServerPid = spawn(fun() -> receive_loop(1) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.


receive_loop(NextNNR) ->
    io:fwrite("receive_loop\n"),
    io:fwrite(?SERVERNAME),
    receive
        {AbsenderPID, getmessages} ->   getmessages_abfertigen(AbsenderPID),
                                        receive_loop(NextNNR);
        {dropmessage, Nachricht} ->     dropmessage_abfertigen(Nachricht),
                                        receive_loop(NextNNR);
        {AbsenderPID, getmsgid} ->  NeueNextNNR = getmsgid_abfertigen(AbsenderPID, NextNNR),
                                    receive_loop(NeueNextNNR)
    end.


getmessages_abfertigen(EmpfaengerPID) ->  
    io:fwrite("Got getmessages"),  
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [1, "Text", TS, TS, TS, TS],
    TerminatedFlag = rand:uniform() > 0.5,
    EmpfaengerPID ! {reply, Nachricht, TerminatedFlag}.


dropmessage_abfertigen(_Nachricht) ->
    io:fwrite("Got dropmessage").


getmsgid_abfertigen(AbsenderPID, LetzteNNR) -> 
    io:fwrite("Got getmsgid"),  
    AbsenderPID ! {nid, LetzteNNR},
    LetzteNNR + 1.



hohle_wert_aus_config_mit_key(Key) ->
    %log_status(extractValueFromConfig,io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.
