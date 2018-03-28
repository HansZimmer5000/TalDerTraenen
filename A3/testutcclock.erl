-module(testutcclock).

-include_lib("eunit/include/eunit.hrl").

test_init_1_test() ->
    OffsetMS = 10,
    ?assertEqual(
        {10},
        utcclock:init(OffsetMS)).

test_adjust_1_test() ->
    UTCClock = {10},
    Messages = [],
    ?assertEqual(
        {10}, 
        utcclock:adjust(UTCClock, Messages)).

get8ByteUTCBinary_1_test() ->
    TS = erlang:timestamp(),
    ShouldResult = vsutil:now2UTC(TS),
    IsResult = binary:decode_unsigned(utcclock:get8ByteUTCBinary(TS), big),
    ?assertEqual(ShouldResult, IsResult).