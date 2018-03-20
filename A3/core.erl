-module(core).

-export([start/0]).

start() ->
    spawn(  fun() -> 
                receiver:myreceive(15000)
            end),
    spawn(  fun() -> 
                receiver:myreceive(15000)
            end),
    sender:mysend().