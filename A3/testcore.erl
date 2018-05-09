-module(testcore).

-include_lib("eunit/include/eunit.hrl").

listen_to_slot_1_test() ->
    ConvertedSlotMessages = [],
    StationWasInvolved = false,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_slot(ThisPid)
                end),
    receive
        Any1 ->
            ?assertEqual(listentoslot, Any1),
            TestPid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}
    end,
    receive
        Any2 ->
            {ReceivedConvertedSlotMessages, ReceivedStationWasInvolved} = Any2,
            ?assertEqual(ConvertedSlotMessages, ReceivedConvertedSlotMessages),
            ?assertEqual(ReceivedStationWasInvolved, StationWasInvolved)
    end.

listen_to_slot_2_test() ->
    ConvertedSlotMessages = [bla],
    StationWasInvolved = true,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_slot(ThisPid)
                end),
    receive
        Any1 ->
            ?assertEqual(listentoslot, Any1),
            TestPid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}
    end,
    receive
        Any2 ->
            {ReceivedConvertedSlotMessages, ReceivedStationWasInvolved} = Any2,
            ?assertEqual(ConvertedSlotMessages, ReceivedConvertedSlotMessages),
            ?assertEqual(ReceivedStationWasInvolved, StationWasInvolved)
    end.

notify_when_preperation_and_send_due_1_test() ->
    ThisPid = self(),
    SlotNumber = 2,
    TestPid = spawn(fun() -> 
                    core:notify_when_preperation_and_send_due(ThisPid, SlotNumber)
                end),

    receive 
        Any1 ->
            {calcslotbeginn, ReceivedSlotNumber, SenderPid} = Any1,
            ?assertEqual(SlotNumber, ReceivedSlotNumber),
            ?assertEqual(TestPid, SenderPid),
            SenderPid ! {resultslotbeginn, 50}
    end,
    receive
        Any2 -> 
            {alarm, AlarmMessage1, TimeMS1} = Any2,
            ?assertEqual(preperation, AlarmMessage1),
            ?assertEqual(40, TimeMS1)
    end,
    receive
        Any3 -> 
            {alarm, AlarmMessage2, TimeMS2} = Any3,
            ?assertEqual(send, AlarmMessage2),
            ?assertEqual(50, TimeMS2)
    end.