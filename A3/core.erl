-module(core).

-export([start/0]).

-define(CLOCKOFFSETMS, 0).

start() ->
    io:fwrite("start"),
    RecvPid = receiver:start(self()),
    SendPid = sender:start(),
    ClockPid = utcclock:start(?CLOCKOFFSETMS),


    RecvPid ! listentoslot,
    SendPid ! {send, "hallo welt"},

    receive_loop().

receive_loop() ->
    receive
        Any -> io:fwrite("Core Got: ~p", [Any])
    end,
    receive_loop().
    