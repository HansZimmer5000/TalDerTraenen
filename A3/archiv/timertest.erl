-module(timertest).

-export([start/0]).


start() ->
    start(100, 0, 0).

start(RestCount, Akku, Count) when RestCount =< 0 ->
    case Count of 
        0 ->
            io:fwrite("No Tests");
        _ ->
            io:fwrite("~p\n", [Akku / Count])
    end;
start(RestCount, Akku, Count) ->
    CycleCount = 10,
    {TmpAkku, TmpCount} = get_offset_average(RestCount, CycleCount, 0, 0),
    start(RestCount - 1, Akku + TmpAkku, Count + TmpCount).

get_offset_average(Number, 0, Akku, Count) ->
    io:fwrite("Offset for ~p is ~p\n", [Number, Akku / Count]),
    {Akku,Count};
get_offset_average(Number, CycleCount, Akku, Count) ->
    StartZeit = set_timer(Number),
    receive
        hello ->
            Offset = vsutil:getUTC() - StartZeit - Number,
            NewAkku = Akku + Offset,
            NewCount = Count + 1,
            get_offset_average(Number, CycleCount - 1, NewAkku, NewCount)
    end.

set_timer(DurationMS) ->
    StartZeit = vsutil:getUTC(), 
    timer:send_after(DurationMS, self(), hello),
    StartZeit.