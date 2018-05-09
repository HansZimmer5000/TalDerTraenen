-module(testutcclock).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULTOFFSETMS, 10).
-define(DEFAULTFRAMECHECKCYLCEMS, 10).

start_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    ThisPid = self(),
    TestPid = utcclock:start(OffsetMS,ThisPid, "testclock.log"),
    TestPid ! {adjust, []},
    TestPid ! {getcurrentoffsetms, self()},
    receive
        10 -> 
            ?assert(true)
        after timer:seconds(1) ->
            ?assert(false)
    end,
    kill_pid_and_clear_this_box(TestPid).

adjust_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Messages = [],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

adjust_2_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "-team-0602-", "123456789012-", 4, 77394825}, 77394825},
    Messages = [Message1],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

adjust_3_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"B", "-team-0602-", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

adjust_4_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 1},
    Message2 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 2},
    Message3 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 3},
    Message4 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 4},
    Message5 = {{"B", "-team-0602-", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1, Message2, Message3, Message4, Message5],
    ?assertEqual(
        OffsetMS + 3, 
        utcclock:adjust(OffsetMS, Messages)).

check_frame_1_test() ->
    ThisPid = self(),
    Starttime = vsutil:getUTC(),
    FrameCount = 0,
    utcclock:check_frame(Starttime, 0, FrameCount, ThisPid),
    receive
        _Any -> 
            ?assert(false)
        after 50 ->
            ?assert(true)
    end.

check_frame_2_test() ->
    ThisPid = self(),
    Starttime = vsutil:getUTC(),
    FrameCount = -1,
    utcclock:check_frame(Starttime, 0, FrameCount, ThisPid),
    receive
        Any -> 
            ?assertEqual(newframe, Any)
        after 50 ->
            ?assert(false)
    end.

new_frame_started_1_test() ->
    CurrentTime = 0,
    FrameCount = -1,
    NewFrameStarted = utcclock:new_frame_started(CurrentTime, FrameCount),
    ?assert(NewFrameStarted).

new_frame_started_2_test() ->
    CurrentTime = 0,
    FrameCount = 0,
    NewFrameStarted = utcclock:new_frame_started(CurrentTime, FrameCount),
    ?assertNot(NewFrameStarted).

new_frame_started_3_test() ->
    CurrentTime = 900,
    FrameCount = 0,
    NewFrameStarted = utcclock:new_frame_started(CurrentTime, FrameCount),
    ?assertNot(NewFrameStarted).

new_frame_started_4_test() ->
    CurrentTime = 1000,
    FrameCount = 0,
    NewFrameStarted = utcclock:new_frame_started(CurrentTime, FrameCount),
    ?assert(NewFrameStarted).

get_current_time_1_test() ->
    Starttime = vsutil:getUTC(),
    OffsetMS = 0,
    ?assertEqual(
        0,
        utcclock:get_current_time(Starttime, OffsetMS)
    ).

get_current_time_2_test() ->
    Starttime = vsutil:getUTC(),
    OffsetMS = 0,
    timer:sleep(100),
    CurrentTime = utcclock:get_current_time(Starttime, OffsetMS),
    ?assert(
        (CurrentTime >= 100) and (CurrentTime < 111) %Sleeping needs some additional time.
    ).

get_current_time_3_test() ->
    Starttime = vsutil:getUTC(),
    OffsetMS = 10,
    ?assertEqual(
        10,
        utcclock:get_current_time(Starttime, OffsetMS)
    ).

calc_slot_beginn_this_frame_time_1_test() ->
    FrameCount = 0,
    SlotNumber = 1,
    ?assertEqual(
        00, 
        utcclock:calc_slot_beginn_this_frame_time(FrameCount, SlotNumber)).

calc_slot_beginn_this_frame_time_2_test() ->
    FrameCount = 1,
    SlotNumber = 25,
    ?assertEqual(
        1960, 
        utcclock:calc_slot_beginn_this_frame_time(FrameCount, SlotNumber)).

set_alarm_1_test() ->
    AlarmMessage = send,
    TimeTillItsDue = 500,
    ThisPid = self(),
    StartFunction = vsutil:getUTC(),
    utcclock:set_alarm(AlarmMessage, TimeTillItsDue, ThisPid),
    receive
        Any ->
            EndFunction = vsutil:getUTC(),
            TimeNeeded = EndFunction - StartFunction,
            ?assertEqual(AlarmMessage, Any),
            ?assert((TimeNeeded >= 500) and (TimeNeeded < 520)) %Timer:send_after in utcclock needs some additional time
    end.

set_alarm_2_test() ->
    AlarmMessage = send,
    TimeTillItsDue = -5,
    ThisPid = self(),
    StartFunction = vsutil:getUTC(),
    utcclock:set_alarm(AlarmMessage, TimeTillItsDue, ThisPid),
    receive
        Any ->
            EndFunction = vsutil:getUTC(),
            TimeNeeded = EndFunction - StartFunction,
            ?assertEqual(AlarmMessage, Any),
            ?assert((TimeNeeded >= 0) and (TimeNeeded < 20)) %Timer:send_after in utcclock needs some additional time
    end.

calc_diff_time_1_test() ->
    CurrentTime = 0,
    SlotBeginnInFrame = 120,
    ?assertEqual(
        -120,
        utcclock:calc_diff_time(CurrentTime, SlotBeginnInFrame)
    ).

calc_diff_time_2_test() ->
    CurrentTime = 120,
    SlotBeginnInFrame = 120,
    ?assertEqual(
        0,
        utcclock:calc_diff_time(CurrentTime, SlotBeginnInFrame)
    ).

get_8_byte_utc_binary_1_test() ->
    TS = erlang:timestamp(),
    ShouldResult = vsutil:now2UTC(TS),
    IsResult = binary:decode_unsigned(utcclock:get_8_byte_utc_binary(TS), big),
    ?assertEqual(ShouldResult, IsResult).


% -------------------

kill_pid_and_clear_this_box(TestPid) ->
    exit(TestPid, kill),
    clear_this_box().

clear_this_box() ->
    receive
        _ -> clear_this_box()
        after 10 -> done
    end.