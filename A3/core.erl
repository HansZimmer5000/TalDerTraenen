-module(core).

-export([start/0]).

start() ->
    RecvPid = receiver:start(self()),
    SendPid = sender:start(),

    SendPid ! {send, "hallo welt"},
    receive
        Any -> io:fwrite("~p", [Any])
    end.
    