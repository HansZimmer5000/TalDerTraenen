-module(testutcclock).

-include_lib("eunit/include/eunit.hrl").

-define(DEFAULTOFFSETMS, 10.0).

test_start_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    TestPid = utcclock:start(OffsetMS),
    TestPid ! {adjust, []},
    TestPid ! {getcurrentoffsetms, self()},
    receive
        Any -> 
            ?assertEqual(OffsetMS, Any)
        after timer:seconds(1) ->
            ?assert(false)
    end.

test_adjust_1_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Messages = [],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

test_adjust_2_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "-team-0602-", "123456789012-", 4, 77394825}, 77394825},
    Messages = [Message1],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

test_adjust_3_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"B", "-team-0602-", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1],
    ?assertEqual(
        OffsetMS, 
        utcclock:adjust(OffsetMS, Messages)).

test_adjust_4_test() ->
    OffsetMS = ?DEFAULTOFFSETMS,
    Message1 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 1},
    Message2 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 2},
    Message3 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 3},
    Message4 = {{"A", "-team-0602-", "123456789012-", 4, 0}, 4},
    Message5 = {{"B", "-team-0602-", "123456789012-", 4, 0}, 77394825},
    Messages = [Message1, Message2, Message3, Message4, Message5],
    ?assertEqual(
        OffsetMS + 2.5, 
        utcclock:adjust(OffsetMS, Messages)).

get_8_byte_utc_binary_1_test() ->
    TS = erlang:timestamp(),
    ShouldResult = vsutil:now2UTC(TS),
    IsResult = binary:decode_unsigned(utcclock:get_8_byte_utc_binary(TS), big),
    ?assertEqual(ShouldResult, IsResult).