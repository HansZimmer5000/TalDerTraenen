
-module(testserver).

-include_lib("eunit/include/eunit.hrl").


getmessages_abfertigen_1_test() ->
    ThisPid = self(),
    ErinnerungsZeit = 2000,
    CMEM = [ErinnerungsZeit, [{ThisPid, 1, erlang:timestamp()}]],
    ServerPid = spawn(fun() -> 
                            NeueCMEM = server:getmessages_abfertigen(ThisPid, CMEM, ThisPid),
                            ThisPid ! NeueCMEM 
                        end),
    receive
        Any1 ->
            {ServerPid, {request, deliverMSG, 2, ThisPid}} = Any1,
            ServerPid ! {reply, 2}
        after 2000 -> ?assert(false)
    end,
    receive
        Any2 -> 
            [ErinnerungsZeit, [{ThisPid, 2, _TS}]] = Any2
        after 2000 -> ?assert(false)
    end,
    fahre_server_vorzeitig_herunter(ServerPid).


dropmessage_abfertigen_1_test() ->
    ThisPid = self(),
    TS = vsutil:now2string(erlang:timestamp()),
    Text =  io_lib:format("~p, ~p, ~p, ~s", [hostname, gruppe2, team6, TS]),
    TestNachricht = [1, Text, TS],
    ServerPid = spawn(fun() -> 
                            Result = server:dropmessage_abfertigen(ThisPid, TestNachricht),
                            ThisPid ! Result
                        end),
    receive
        Any1 -> 
            {ServerPid, {request, pushHBQ, TestNachricht}} = Any1,
            ServerPid ! {reply, ok}
        after 2000 -> ?assert(false)
    end,
    receive
        Any2 -> ?assertEqual(ok, Any2)
        after 2000 -> ?assert(false)
    end,
    fahre_server_vorzeitig_herunter(ServerPid).

getmsgid_abfertigen_1_test() ->
    AktuelleNNR = server:getmsgid_abfertigen(self(), 0),
    receive 
        {nid, 1} -> Temp_Ergebnis = true;
        _Any -> Temp_Ergebnis = false
    end,
    AktuelleNNR = 1,
    Temp_Ergebnis.


fahre_server_vorzeitig_herunter(ServerPid) ->
    exit(ServerPid, 'test vorbei').