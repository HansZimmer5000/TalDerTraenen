-module(tunnel).

-export(
    [
        test/0,
        recv/1,
        send/0
    ]).

-define(LIFETIME, 10000).

-define(PRAKTIKUMMCADDR, {224,0,0,251}).%{225,0,10,1}).
-define(PRAKTIKUMPORT, 15006).

-define(HOMEMCADDR, {239,1,1,3}).
-define(HOMEPORT, 15006).

test() ->
    spawn(fun() -> receive0() end),
    spawn(fun() -> receive0() end),
    timer:sleep(timer:seconds(1)),
    spawn(fun() -> send0() end),
    spawn(fun() -> send0() end).

receive0() ->
    {ok, Socket} = gen_udp:open(5353, [
        {mode, binary},
        {reuseaddr, true},
        {ip, {224, 0, 0, 251}},
        {multicast_ttl, 4},
        {multicast_loop, true},
        {broadcast, true},
        {add_membership, {{224, 0, 0, 251}, {0, 0, 0, 0}}},
        {active, false}]), %once = dann mit receive Any -> ... end holen

    gen_udp:controlling_process(Socket, self()),
    io:fwrite("receiving"),

    % Egal welche Abholung -> Die Nachrichten werden wohl nicht gehalten sondern nur neue werden gesichtet.
    % TODO: mal mit Socket erstellung und weitergebung machen.
    %receive {udp, _Socket0, _Ip0, _Port0, Message} -> io:fwrite("~p", [Message]) end,
    %receive Message -> io:fwrite("~p", [Message]) end,
    io:fwrite("~p", [gen_udp:recv(Socket, 0)]),
    receive0().

send0() ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    gen_udp:send(Socket, {224, 0, 0, 251}, 5353, <<"hello world">>),
    io:fwrite("sended").
            
send() ->
    MultiCastAddr = ?HOMEMCADDR,
    Port = ?HOMEPORT,
    Source = {10,0,112,251},

    Options = [
        binary, inet,
        {active, true}, %{multicast_if, MultiCastAddr},
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
