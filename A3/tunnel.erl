-module(tunnel).

-export(
    [
        test/0
    ]).

-define(LIFETIME, 10000).

-define(PRAKTIKUMMCADDR, {225,0,10,1}).
-define(PRAKTIKUMPORT, 15006).

-define(HOMEMCADDR, {239,1,1,3}).
-define(HOMEPORT, 15006).

test() ->
    test1(), 
    io:fwrite("\n\nTest2\n\n"),
    test2().

test1() ->
    spawn(fun() -> recv({0,0,0,0}) end),
    timer:sleep(timer:seconds(1)), 
    spawn(fun() -> send() end).

test2() ->
    spawn(fun() -> recv({0,0,0,0}) end),
    timer:sleep(timer:seconds(1)), 
    spawn(fun() -> recv({10,0,112,251}) end),
    timer:sleep(timer:seconds(1)), 
    spawn(fun() -> send() end),
    timer:sleep(timer:seconds(1)),
    spawn(fun() -> send() end),
    nothing.

send() ->
    MultiCastAddr = ?HOMEMCADDR,
    Port = ?HOMEPORT,
    Source = {10,0,112,251},

    Options = [
        binary, inet, -
        {active, false}, %{multicast_if, MultiCastAddr},
        {add_membership, {MultiCastAddr, Source}},
        {ip, Source}, {multicast_ttl, 1}, {multicast_loop, true}
    ],
    {ok, Socket} = gen_udp:open(Port, Options),

    ok = gen_udp:send(Socket, MultiCastAddr, Port, <<"TextNachricht">>),

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).

recv(Addr) ->
    MultiCastAddr = ?HOMEMCADDR,
    Port = ?HOMEPORT,

    Options = [
        binary, inet,	
        {active, true},
        {multicast_ttl, 1}, 
        {multicast_loop, true}, {add_membership, {MultiCastAddr, Addr}}
    ],
    {ok, Socket} = gen_udp:open(Port, Options),
    gen_udp:controlling_process(Socket, self()),
    receive Any -> io:fwrite("~p", [Any]) end,

    timer:apply_after(?LIFETIME, gen_udp, close, [Socket]).
