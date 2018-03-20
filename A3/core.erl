-module(core).

-export([start/0]).

start() ->
    ThisPid = self(),
    spawn(  fun() -> 
                receiver:myreceive()
            end),
    sender:mysend().