-module(core).

-export([start/0]).

start() ->
    spawn(  fun() -> 
                receiver:myreceive()
            end),
    sender:mysend().