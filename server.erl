
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
    ServerPid = spawn(fun() -> receive_loop(0) end),
    register(?SERVERNAME, ServerPid),
    ServerPid.


receive_loop(LetzteNNR) ->
    receive
        {AbsenderPID, getmessages} ->   getmessages_abfertigen(AbsenderPID),
                                        receive_loop(LetzteNNR);
        {dropmessage, Nachricht} ->     dropmessage_abfertigen(Nachricht),
                                        receive_loop(LetzteNNR);
        {AbsenderPID, getmsgid} ->  NeueLetzteNNR = getmsgid_abfertigen(AbsenderPID, LetzteNNR),
                                    receive_loop(NeueLetzteNNR)
    end.


getmessages_abfertigen(EmpfaengerPID) ->    
    TS = werkzeug:now2string(erlang:timestamp()),
    Nachricht = {1, "Text", TS, TS, TS, TS},
    TerminatedFlag = rand:uniform() > 0.5,
    EmpfaengerPID ! {reply, Nachricht, TerminatedFlag}.


dropmessage_abfertigen(_Nachricht) ->
    io:fwrite("Got dropmessage").


getmsgid_abfertigen(EmpfaengerPID, LetzteNNR) -> 
    EmpfaengerPID ! {nid, LetzteNNR},
    LetzteNNR + 1.



hohle_wert_aus_config_mit_key(Key) ->
    %log_status(extractValueFromConfig,io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = werkzeug:get_config_value(Key, ConfigListe),
    Value.
