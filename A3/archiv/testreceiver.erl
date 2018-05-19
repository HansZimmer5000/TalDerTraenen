-module(testreceiver).

-include_lib("eunit/include/eunit.hrl").

start_1_test() ->
    StationName = "Station1",
    ThisPid = self(),
    CorePid = self(),
    InterfaceNameAtom = '127.0.0.1',
    McastAddressAtom = '224.0.0.251',
    ReceivePort = 15006,
    LogFile = "testreceiver.log",
    TestPid = receiver:start(CorePid, StationName, InterfaceNameAtom, McastAddressAtom, ReceivePort,LogFile),
    TestPid ! listentoslot,
    receive
        Any ->
            {slotmessages, SlotMessages, StationWasInvolved} = Any,
            ?assertEqual([], SlotMessages),
            ?assertNot(StationWasInvolved)
        after timer:seconds(41) ->
            ?assert(false)
    end.

create_socket_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

listen_to_slot_1_test() ->
    StationName = "Station1",
    SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
    Message2AsByte = <<"Ateam 06-021234567890123-", 25, SendTimeBinary/binary>>,
    ThisPid = self(),
    CorePid = ThisPid,
    InterfaceNameAtom = '0,0,0,0',
    McastAddressAtom = '224.0.0.251',
    ReceivePort = 15006,
    Socket = receiver:create_socket(InterfaceNameAtom, McastAddressAtom, ReceivePort),
    LogFile = "testreceiver.log",
    _TestPid = spawn(fun() ->
                        receiver:listen_to_slot(CorePid, StationName, Socket, LogFile)
                    end),
    io:fwrite("TODO: Send message!"),
    ?assert(false),

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
        CorePid = ThisPid,
        InterfaceNameAtom = '0,0,0,0',
        McastAddressAtom = '224.0.0.251',
        ReceivePort = 15006,
        Socket = receiver:create_socket(InterfaceNameAtom, McastAddressAtom, ReceivePort),
        LogFile = "testreceiver.log",
        StartZeit = vsutil:getUTC(),
        _TestPid = spawn(fun() ->
                            receiver:listen_to_slot(CorePid, StationName, Socket, LogFile)
                        end),
        receive 
            Any -> 
                EndZeit = vsutil:getUTC(),
                {slotmessages, ConvertedMessages, StationWasInvolved} = Any,
                ?assertEqual([], ConvertedMessages),
                ?assertEqual(false, StationWasInvolved),
                DiffZeit = EndZeit - StartZeit,
                io:fwrite("~p, ~p, ~p", [DiffZeit, StartZeit rem 10000, EndZeit rem 10000]),
                ?assert((DiffZeit >= 40) and (DiffZeit =< 41))
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

collision_happend_1_test() ->
    ConvertedSlotMessages = [],
    StationName = "team 06-02",
    LogFile = "testreceiver.log",
    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedSlotMessages, StationName, LogFile),
    ?assertNot(CollisionHappend),
    ?assertNot(StationWasInvolved).

collision_happend_2_test() ->
    ConvertedSlotMessages = [
        {{"A", "team 06-01", "-1234567890123", 24, empty}, empty},
        {{"A", "team 06-03", "-1234567890123", 24, empty}, empty}
    ],
    StationName = "team 06-02",
    LogFile = "testreceiver.log",
    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedSlotMessages, StationName, LogFile),
    ?assert(CollisionHappend),
    ?assertNot(StationWasInvolved).

collision_happend_3_test() ->
    ConvertedSlotMessages = [
        {{"A", "team 06-01", "-1234567890123", 25, empty}, empty},
        {{"A", "team 06-03", "-1234567890123", 24, empty}, empty}
    ],
    StationName = "team 06-02",
    LogFile = "testreceiver.log",
    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedSlotMessages, StationName, LogFile),
    ?assert(CollisionHappend),
    ?assertNot(StationWasInvolved).

collision_happend_4_test() ->
    ConvertedSlotMessages = [
        {{"A", "team 06-02", "-1234567890123", 24, empty}, empty},
        {{"A", "team 06-03", "-1234567890123", 24, empty}, empty}
    ],
    StationName = "team 06-02",
    LogFile = "testreceiver.log",
    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedSlotMessages, StationName, LogFile),
    ?assert(CollisionHappend),
    ?assert(StationWasInvolved).


send_to_core_1_test() ->
    ConvertedSlotMessages = [],
    CollisionHappend = false,
    StationWasInvolved = false,
    CorePid = self(),
    LogFile = "testreceiver.log",
    receiver:send_to_core(ConvertedSlotMessages, CollisionHappend, StationWasInvolved, CorePid, LogFile),
    receive
        Any ->
            ?assertEqual({slotmessages, ConvertedSlotMessages, StationWasInvolved}, Any)
    end.

send_to_core_2_test() ->
    ConvertedSlotMessages = [
        {{"A", "team 06-02", "-1234567890123", 24, empty}, empty},
        {{"A", "team 06-03", "-1234567890123", 24, empty}, empty}
    ],
    CollisionHappend = true,
    StationWasInvolved = true,
    CorePid = self(),
    LogFile = "testreceiver.log",
    receiver:send_to_core(ConvertedSlotMessages, CollisionHappend, StationWasInvolved, CorePid, LogFile),
    receive
        Any ->
            ?assertEqual({slotmessages, [], StationWasInvolved}, Any)
    end.

    