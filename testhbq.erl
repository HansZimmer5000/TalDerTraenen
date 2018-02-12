
-module(testhbq).

-include_lib("eunit/include/eunit.hrl").

start_1_test() ->
    HBQPid = hbq:start(),
    HBQPid ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> ok
        after 5 -> true = false
    end,
    exit(HBQPid, kill).