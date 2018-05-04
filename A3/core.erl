-module(core).

-export([start/0]).

start() ->
    io:fwrite("start"),
    RecvPid = receiver:start(self()),
    SendPid = sender:start(),



    RecvPid ! listen_to_slot,
    SendPid ! {send, "hallo welt"},

    receive_loop().

receive_loop() ->
    receive
        Any -> io:fwrite("Core Got: ~p", [Any])
    end,
    receive_loop().
    