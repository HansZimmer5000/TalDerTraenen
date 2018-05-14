-module(testcore).

-include_lib("eunit/include/eunit.hrl").

frame_loop_1_test() ->
    StationName = "team 06-01",
    StationType = "A",
    FrameNumber = 0,
    SendingSlotNumber = 23,
    ClockOffsetMS = 0,
    LogFile = "testcore.log",
    ThisPid = self(),
    PayloadServerPid = spawn(fun() -> 
            receive 
                    AnyPyld -> 
                        {Sender, getNextPayload} = AnyPyld,
                        Sender ! {payload, "team 06-01-1234567890123"}
            end
        end),
    SendPid = ThisPid,

    StartZeit = vsutil:getUTC(),
    CorePid = spawn(fun() -> 
            CorePid = self(),
            RecvPid = spawn(fun() -> ThisPid ! answer_for_empty_frame(CorePid) end),
            ClockPid = utcclock:start(ClockOffsetMS, CorePid, StationName, LogFile),
            core:frame_loop(StationName, StationType, FrameNumber, SendingSlotNumber, {RecvPid, SendPid, ClockPid, PayloadServerPid}, LogFile)
        end),

    receive
        Any1 ->
            {send, Message} = Any1,
            exit(CorePid, kill),

            [ConvertedMessage] = messagehelper:convert_received_messages_from_byte([Message], [vsutil:getUTC()]),
            ?assertEqual(34, byte_size(Message))            
            %exit(RecvPid, kill),
            %exit(ClockPid, kill)
        after 1 ->
            ?assert(false)
    end,

    receive
        Any2 ->
            done = Any2
        after 1 ->
            ?assert(false)
    end,

    EndZeit = vsutil:getUTC(),
    DiffZeit = EndZeit - StartZeit - 1000,
    io:fwrite("~p", [DiffZeit]),
    ?assert((DiffZeit == 0) or (DiffZeit == 1)).

listen_to_frame_1_test() ->
    ConvertedSlotMessages = [1,2,3,4,5,6,7,8,9,11,12,13,14,15,16,17,19,20,21,22,23,24,25],
    StationWasInvolved = false,
    ThisPid = self(),
    RecvPid = ThisPid,
    FrameStart = 0,
    LogFile = "testcore.log",
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_frame(RecvPid, FrameStart, LogFile)
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
    RecvPid = ThisPid,
    FrameStart = 0,
    LogFile = "testcore.log",
    TestPid = spawn(fun() ->
                    ThisPid ! core:listen_to_frame(RecvPid, FrameStart, LogFile)
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
    ClockPid = ThisPid,
    FrameStart = 0,
    SlotNumber = 2,
    LogFile = "testcore.log",
    TestPid = spawn(fun() -> 
                    core:notify_when_preperation_and_send_due(ClockPid, FrameStart, SlotNumber, LogFile)
                end),

    receive 
        Any1 ->
             %Simulatin the Clock here.
            {calcslotmid, FrameStart, ReceivedSlotNumber, SenderPid} = Any1,
            ?assertEqual(SlotNumber, ReceivedSlotNumber),
            ?assertEqual(TestPid, SenderPid),
            SenderPid ! {resultslotmid, 50}
    end,
    receive
        Any2 -> 
            {alarm, AlarmMessage1, TimeMS1, SenderPid1} = Any2,
            ?assertEqual(preperation, AlarmMessage1),
            ?assertEqual(30, TimeMS1),
            ?assertEqual(TestPid, SenderPid1)
    end,
    receive
        Any3 -> 
            {alarm, AlarmMessage2, TimeMS2, SenderPid2} = Any3,
            ?assertEqual(send, AlarmMessage2),
            ?assertEqual(40, TimeMS2),
            ?assertEqual(TestPid, SenderPid2)
    end.

start_sending_process_1_test() ->
    SendPid = self(),
    FrameStart = vsutil:getUTC(),
    SlotNumber = 1,
    StationType = "A",
    StationName = "team 06-01",
    ClockOffsetMS = 0,
    LogFile = "testcore.log",
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    PayloadServerPid = self(),

    core:start_sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile),

    receive
        Any1 ->
            {Sender, getNextPayload} = Any1,
            Sender ! {payload, "-1234567890123"}
    end,

    receive
        Any2 ->
            {send, Message} = Any2
    end,

    receive 
        Any3 ->
            {messagewassend, MessageWasSend} = Any3,
            ?assert(MessageWasSend)
    end.
% ----------------

receive_listentoslot_and_answer(SenderPid, Messages, StationWasInvolved)->
    receive 
        listentoslot ->
            SenderPid ! {slotmessages, Messages, StationWasInvolved}
    end.

answer_for_empty_frame(CorePid) ->
    answer_for_empty_frame(25, CorePid).

answer_for_empty_frame(0, _CorePid) ->
    done;
answer_for_empty_frame(RestCount, CorePid) ->
    receive 
        listentoslot ->
            timer:sleep(39),
            CorePid ! {slotmessages, [], false}
    end,
    answer_for_empty_frame(RestCount - 1).