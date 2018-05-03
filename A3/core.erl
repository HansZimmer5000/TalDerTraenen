-module(core).

-export([start/0]).

start() ->
    RecvPid = receiver:start(self()),
    SendPid = sender:start(),

    SendPid ! {send, "hallo welt"},

    receive_loop().

receive_loop() ->
    receive
        Any -> io:fwrite("Core Got: ~p", [Any])
    end,
    receive_loop().
    