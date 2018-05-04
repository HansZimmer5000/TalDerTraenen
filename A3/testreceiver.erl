-module(testreceiver).

-include_lib("eunit/include/eunit.hrl").

start_1_test() ->
    ThisPid = self(),
    TestPid = receiver:start(ThisPid, ThisPid),
    receive
        Any ->
            {enlist, Pid} = Any,
            ?assertEqual(TestPid, Pid)
        after timer:seconds(1) ->
            ?assert(false)
    end.

listen_to_slot_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:listen_to_slot(ThisPid)
                    end),
    TestPid ! {udp, empty, empty, empty, <<"Hallo Welt">>},

    receive 
        Any -> 
            {slotmessages, Messages, ReceivedTimes} = Any,
            [Message] = Messages,
            [RecvTime] = ReceivedTimes,
            DiffRecvTime = vsutil:diffTS(erlang:timestamp(), RecvTime),
            {0,0,_} = DiffRecvTime,
            ?assertEqual(<<"Hallo Welt">>, Message)
        after timer:seconds(1) -> 
            ?assert(false)
    end.

listen_to_slot_2_test() ->
    ThisPid = self(),
    _TestPid = spawn(fun() ->
                        receiver:listen_to_slot(ThisPid)
                    end),
    receive 
        Any -> 
            {slotmessages, Messages, ReceivedTimes} = Any,
            ?assertEqual([], Messages),
            ?assertEqual([], ReceivedTimes)
        after timer:seconds(1) -> 
            ?assert(false)
    end.

loop_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:loop(ThisPid)
                    end),

    TestPid ! {udp, empty, empty, empty, <<"Hallo Welt">>},

    receive
        _Any ->
            ?assert(false)
        after timer:seconds(1) ->
            ?assert(true)
    end.

loop_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:loop(ThisPid)
                    end),
    TestPid ! {udp, empty, empty, empty, <<"Hallo Welt">>},            
    TestPid ! listentoslot,
    TestPid ! {udp, empty, empty, empty, <<"Hallo Welt2">>},

    receive 
        Any -> 
            {slotmessages, Messages, ReceivedTimes} = Any,
            [Message] = Messages,
            [RecvTime] = ReceivedTimes,
            DiffRecvTime = vsutil:diffTS(erlang:timestamp(), RecvTime),
            {0,0,_} = DiffRecvTime,
            ?assertEqual(<<"Hallo Welt2">>, Message)
        after timer:seconds(1) -> 
            ?assert(false)
    end.