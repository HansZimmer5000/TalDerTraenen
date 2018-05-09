-module(testutcclock).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULTOFFSETMS, 10).
-define(DEFAULTFRAMECHECKCYLCEMS, 10).

start_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    ThisPid = self(),
    TestPid = utcclock:start(OffsetMS,ThisPid),
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
    utcclock:check_frame(Starttime, 0, 10, ThisPid),
    receive
        Any -> 
            ?assertEqual(newframe, Any)
        after 50 ->
            ?assert(false)
    end.

check_frame_2_test() ->
    ThisPid = self(),
    Starttime = vsutil:getUTC(),
    timer:sleep(30),
    utcclock:check_frame(Starttime, 0, 10, ThisPid),
    receive
        _Any -> 
            ?assert(false)
        after 50 ->
            ?assert(true)
    end.

new_frame_started_1_test() ->
    Starttime = vsutil:getUTC(),
    NewFrameStarted = utcclock:new_frame_started(Starttime, 0, ?DEFAULTFRAMECHECKCYLCEMS),
    ?assert(NewFrameStarted).

new_frame_started_2_test() ->
    Starttime = vsutil:getUTC(),
    timer:sleep(30),
    NewFrameStarted = utcclock:new_frame_started(Starttime, 0, ?DEFAULTFRAMECHECKCYLCEMS),
    ?assertNot(NewFrameStarted).

new_frame_started_3_test() ->
    Starttime = vsutil:getUTC(),
    OffsetMS = ?DEFAULTFRAMECHECKCYLCEMS,
    NewFrameStarted = utcclock:new_frame_started(Starttime, OffsetMS, ?DEFAULTFRAMECHECKCYLCEMS),
    ?assertNot(NewFrameStarted).

new_frame_started_4_test() ->
    Starttime = vsutil:getUTC(),
    OffsetMS = ?DEFAULTFRAMECHECKCYLCEMS * - 1,
    NewFrameStarted = utcclock:new_frame_started(Starttime, OffsetMS, ?DEFAULTFRAMECHECKCYLCEMS),
    ?assertNot(NewFrameStarted).

new_frame_started_5_test() ->
    Starttime = vsutil:getUTC() + ?DEFAULTFRAMECHECKCYLCEMS,
    OffsetMS = 0,
    NewFrameStarted = utcclock:new_frame_started(Starttime, OffsetMS, ?DEFAULTFRAMECHECKCYLCEMS),
    ?assertNot(NewFrameStarted).

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