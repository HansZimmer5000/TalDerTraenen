-module(nameservice).

-export([
    start/0
]).

-define(NSNAME, nameservice).

start() ->
    Pid = spawn(fun() -> ok end),
    register(?NSNAME, Pid).