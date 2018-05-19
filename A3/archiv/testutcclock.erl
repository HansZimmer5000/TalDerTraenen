-module(testutcclock).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULTOFFSETMS, 10).
-define(DEFAULTFRAMECHECKCYLCEMS, 10).

start_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    ThisPid = self(),
    CorePid = ThisPid,
    StationName = "team 06-01",
    LogFile = "testutc.log",
    TestPid = utcclock:start(OffsetMS, CorePid, StationName, LogFile),
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
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {OffsetMS, TransportTupel}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

adjust_2_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "team 06-02", "123456789012-", 4, 77394825}, 77394825},
    Messages = [Message1],
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {0, TransportTupel}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

adjust_3_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"B", "team 06-02", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1],
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {OffsetMS, TransportTupel}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

adjust_4_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "team 06-02", "123456789012-", 4, 0}, 1},
    Message2 = {{"A", "team 06-02", "123456789012-", 4, 0}, 2},
    Message3 = {{"A", "team 06-02", "123456789012-", 4, 0}, 3},
    Message4 = {{"A", "team 06-02", "123456789012-", 4, 0}, 4},
    Message5 = {{"B", "team 06-02", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1, Message2, Message3, Message4, Message5],
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {-3, TransportTupel}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

adjust_5_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "team 06-01", "123456789012-", 4, 0}, -?DEFAULTOFFSETMS + 1},
    Message2 = {{"A", "team 06-02", "123456789012-", 4, 0}, 2},
    Messages = [Message1, Message2],
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {4, {"team 06-01", 1, 1}}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

adjust_6_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "team 06-02", "123456789012-", 4, 0}, 2},
    Message2 = {{"A", "team 06-01", "123456789012-", 4, 0}, -?DEFAULTOFFSETMS + 1},
    Messages = [Message1, Message2],
    TransportTupel = {"team 06-01", 0, 0},
    LogFile = "testutc.log",
    ?assertEqual(
        {4, {"team 06-01", 1, 1}}, 
        utcclock:adjust(OffsetMS, Messages, TransportTupel, LogFile)).

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
    OffsetMS = 0,
    CurrentTime = utcclock:get_current_time(OffsetMS),
    Now = vsutil:getUTC(),
    Diff = Now - CurrentTime,
    io:fwrite("~p", [Diff]),
    ?assert(
        (Diff >= 0) and (Diff =< 1) %Sleeping needs some additional time.
    ).

get_current_time_2_test() ->
    OffsetMS = 0,
    CurrentTime = utcclock:get_current_time(OffsetMS),
    timer:sleep(100),
    Now = vsutil:getUTC(),
    Diff = Now - CurrentTime,
    io:fwrite("~p", [Diff]),
    ?assert(
        (Diff >= 100) or (Diff =< 102) %Sleeping needs some additional time.
    ).

get_current_time_3_test() ->
    OffsetMS = 10,
    CurrentTime = utcclock:get_current_time(OffsetMS),
    Now = vsutil:getUTC(),
    Diff = Now - CurrentTime,
    io:fwrite("~p", [Diff]),
    ?assert(
        (Diff == -10) or (Diff == -9) %Sleeping needs some additional time.
    ).
calc_slot_mid_this_frame_time_1_test() ->
    FrameStart = 0,
    SlotNumber = 1,
    LogFile = "testutc.log",
    SlotMid = utcclock:calc_slot_mid_this_frame_time(FrameStart, SlotNumber, LogFile),
    Diff = SlotMid - 20 - FrameStart,
    io:fwrite("~p", [Diff]),
    ?assert((Diff == 0) or (Diff == 1)).

calc_slot_mid_this_frame_time_2_test() ->
    FrameStart = 7,
    SlotNumber = 25,
    LogFile = "testutc.log",
    SlotMid = utcclock:calc_slot_mid_this_frame_time(FrameStart, SlotNumber, LogFile),
    Diff = SlotMid - 980 - FrameStart,
    io:fwrite("~p", [Diff]),
    ?assert((Diff == 0) or (Diff == 1)).

set_alarm_1_test() ->
    AlarmMessage = send,
    TimeTillItsDue = 500,
    ThisPid = self(),
    StartFunction = vsutil:getUTC(),
    LogFile = "testutc.log",
    utcclock:set_alarm(AlarmMessage, TimeTillItsDue, ThisPid, LogFile),
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
    LogFile = "testutc.log",
    utcclock:set_alarm(AlarmMessage, TimeTillItsDue, ThisPid, LogFile),
    receive
        Any ->
            EndFunction = vsutil:getUTC(),
            TimeNeeded = EndFunction - StartFunction,
            ?assertEqual(AlarmMessage, Any),
            ?assert((TimeNeeded >= 0) and (TimeNeeded < 20)) %Timer:send_after in utcclock needs some additional time
    end.


% -------------------

kill_pid_and_clear_this_box(TestPid) ->
    exit(TestPid, kill),
    clear_this_box().

clear_this_box() ->
    receive
        _ -> clear_this_box()
        after 10 -> done
    end.