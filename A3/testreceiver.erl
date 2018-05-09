-module(testreceiver).

-include_lib("eunit/include/eunit.hrl").

start_1_test() ->
    StationName = "Station1",
    ThisPid = self(),
    TestPid = receiver:start(ThisPid, StationName, "testrecv.log", ThisPid),
    receive
        Any ->
            {enlist, Pid} = Any,
            ?assertEqual(TestPid, Pid)
        after timer:seconds(1) ->
            ?assert(false)
    end.

listen_to_slot_1_test() ->
    StationName = "Station1",
    SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
    Message2AsByte = <<"Ateam 06-021234567890123-", 25, SendTimeBinary/binary>>,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:listen_to_slot(ThisPid, StationName, "testrecv.log")
                    end),
    TestPid ! {udp, empty, empty, empty, Message2AsByte},

    receive 
        Any -> 
            {slotmessages, ConvertedMessages, StationWasInvolved} = Any,
            [ConvertedMessage] = ConvertedMessages,
            false = StationWasInvolved,
            DiffRecvTime = messagehelper:get_receivedtime(ConvertedMessage) - vsutil:getUTC(),
            ?assert(DiffRecvTime < 20),
            ?assertEqual(Message2AsByte, messagehelper:convert_message_to_byte(ConvertedMessage))
        after timer:seconds(1) -> 
            ?assert(false)
    end.

listen_to_slot_2_test() ->
    StationName = "Station1",
    ThisPid = self(),
    _TestPid = spawn(fun() ->
                        receiver:listen_to_slot(ThisPid, StationName, "testrecv.log")
                    end),
    receive 
        Any -> 
            {slotmessages, ConvertedMessages, StationWasInvolved} = Any,
            ?assertEqual([], ConvertedMessages),
            ?assertEqual(false, StationWasInvolved)
        after timer:seconds(1) -> 
            ?assert(false)
    end.

loop_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:loop(ThisPid, "Station1", "testrecv.log")
                    end),

    TestPid ! {udp, empty, empty, empty, <<"Hallo Welt">>},

    receive
        _Any ->
            ?assert(false)
        after timer:seconds(1) ->
            ?assert(true)
    end.

loop_2_test() ->
    StationName = "Station1",
    SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
    Message1AsByte = <<"Ateam 06-021234567890123-", 4, SendTimeBinary/binary>>,
    Message2AsByte = <<"Ateam 06-021234567890123-", 25, SendTimeBinary/binary>>,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        receiver:loop(ThisPid, StationName, "testrecv.log")
                    end),
    TestPid ! {udp, empty, empty, empty, Message1AsByte},            
    TestPid ! listentoslot,
    TestPid ! {udp, empty, empty, empty, Message2AsByte},

    receive 
        Any -> 
            {slotmessages, ConvertedMessages, StationWasInvolved} = Any,
            [ConvertedMessage] = ConvertedMessages,
            false = StationWasInvolved,
            DiffRecvTime = messagehelper:get_receivedtime(ConvertedMessage) - vsutil:getUTC(),
            ?assert(DiffRecvTime < 20),
            ?assertEqual(Message2AsByte, messagehelper:convert_message_to_byte(ConvertedMessage))
        after timer:seconds(1) -> 
            ?assert(false)
    end.

loop_3_test() ->
        StationName = "team 06-02",
        SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
        Message1AsByte = <<"Ateam 06-021234567890123-", 4, SendTimeBinary/binary>>,
        Message2AsByte = <<"Ateam 06-021234567890123-", 25, SendTimeBinary/binary>>,
        ThisPid = self(),
        TestPid = spawn(fun() ->
                            receiver:loop(ThisPid, StationName, "testrecv.log")
                        end),
        TestPid ! {udp, empty, empty, empty, Message1AsByte},            
        TestPid ! listentoslot,
        TestPid ! {udp, empty, empty, empty, Message1AsByte}, 
        TestPid ! {udp, empty, empty, empty, Message2AsByte},
    
        receive 
            Any -> 
                {slotmessages, ConvertedMessages, StationWasInvolved} = Any,
                ?assertEqual(ConvertedMessages, []),
                ?assert(StationWasInvolved)
            after timer:seconds(1) -> 
                ?assert(false)
        end.