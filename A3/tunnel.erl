-module(tunnel).

-export(
    [
        test/0,
        send/0,
        recv/0
    ]).

-define(LIFETIME, 10000).

-define(PRAKTIKUMMCADDR, {225,0,10,1}).
-define(PRAKTIKUMPORT, 15006).

-define(HOMEMCADDR, {239,1,1,3}).
-define(HOMEPORT, 15006).

test() ->
    spawn(fun() -> recv() end),
    timer:sleep(timer:seconds(1)), 
    %spawn(fun() -> recv() end),
    timer:sleep(timer:seconds(1)), 
    io:fwrite("\n\n"), spawn(fun() -> send() end).

send() ->
    io:fwrite("Starting Sending"),
    MultiCastAddr = ?HOMEMCADDR,
    Port = ?HOMEPORT,
    Source = {10,0,112,251},

    Options = [
        binary, inet, 
        {active,false}, {multicast_if, MultiCastAddr},
        {reuseaddr,true}, {add_membership, {MultiCastAddr, Source}},
        {ip, Source}, {multicast_ttl, 1}, {multicast_loop, true}
    ],
    {ok, Socket} = gen_udp:open(Port, Options),

    io:fwrite("\nSending now"),
    ok = gen_udp:send(Socket, MultiCastAddr, Port, <<"TextNachricht">>),

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).

recv() ->
    io:fwrite("Starting Receiving"),
    MultiCastAddr = ?HOMEMCADDR,
    Port = ?HOMEPORT,
    Addr = {0,0,0,0},

    Options = [
        binary, inet,	
        {active, false}, 
        {reuseaddr, true}, 
        {multicast_ttl, 1}, 
        {multicast_loop, true}, {add_membership, {MultiCastAddr, Addr}}
    ],
    {ok, Socket} = gen_udp:open(Port, Options),
    gen_udp:controlling_process(Socket, self()),

    io:fwrite("\nReceiving now"),
    {ok, {Address, Port, Packet}} = gen_udp:recv(Socket, 0),
    io:fwrite("Got: ~p, ~p, ~p", [Address, Port, Packet]),

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).
