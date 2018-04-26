-module(receiver).

-export([
    myreceive/0
    ]).

-define(LIFETIME, 1000).


myreceive() ->
    MultiCastAddr = {225,0,10,1},
    Addr = {0,0,0,0}, 
    Port = 15006,

    Socket = vsutil:openRec(MultiCastAddr, Addr, Port),
    gen_udp:controlling_process(Socket, self()),
    {ok, {Address, Port, Packet}} = gen_udp:recv(Socket, 0),
    io:fwrite("Got: ~p, ~p, ~p", [Address, Port, Packet]),

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).