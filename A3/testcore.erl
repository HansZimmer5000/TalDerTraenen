-module(testcore).

-include_lib("eunit/include/eunit.hrl").

listen_to_frame_1_test() ->
    ConvertedSlotMessages = [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,19,20,21,22,23,24,25],
    StationWasInvolved = false,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_frame(ThisPid)
                end),
    receive_listentoslot_and_answer(TestPid, [1], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [2], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [3], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [4], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [5], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [6], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [7], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [8], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [9], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [11], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [12], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [13], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [14], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [15], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [16], true),
    receive_listentoslot_and_answer(TestPid, [17], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [19], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [20], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [21], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [22], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [23], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [24], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [25], StationWasInvolved),

    receive
        Any2 ->
            {ReceivedConvertedSlotMessages, ReceivedStationWasInvolved} = Any2,
            ?assertEqual(ConvertedSlotMessages, ReceivedConvertedSlotMessages),
            ?assertEqual(true, ReceivedStationWasInvolved)
    end.

listen_to_frame_2_test() ->
    ConvertedSlotMessages = [1,2,3,4,5,6,25],
    StationWasInvolved = false,
    ThisPid = self(),
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_frame(ThisPid)
                end),
    receive_listentoslot_and_answer(TestPid, [1], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [2], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [3], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [4], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [5], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [6], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [], StationWasInvolved),
    receive_listentoslot_and_answer(TestPid, [25], StationWasInvolved),

    receive
        Any2 ->
            {ReceivedConvertedSlotMessages, ReceivedStationWasInvolved} = Any2,
            ?assertEqual(ConvertedSlotMessages, ReceivedConvertedSlotMessages),
            ?assertEqual(false, ReceivedStationWasInvolved)
    end.

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
                    core:notify_when_preperation_and_send_due(ThisPid, SlotNumber, "testcore.log")
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


% ----------------

receive_listentoslot_and_answer(SenderPid, Messages, StationWasInvolved)->
    receive 
        listentoslot ->
            SenderPid ! {slotmessages, Messages, StationWasInvolved}
    end.
