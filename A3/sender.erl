-module(sender).

-export(
    [
        mysend/0
    ]).

-define(LIFETIME, 10000).

mysend() ->
    MultiCastAddr = {225,0,10,1},
    Port = 15006,

    Socket = vsutil:openSe(MultiCastAddr, Port),
    gen_udp:controlling_process(Socket, self()),
    gen_udp:send(Socket, MultiCastAddr, Port, <<"TextNachricht">>),

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).

