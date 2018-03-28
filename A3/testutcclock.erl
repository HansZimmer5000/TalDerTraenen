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
