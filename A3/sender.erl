-module(sender).

-export(
    [
        mysend/0, 
        mysend/1
    ]).

mysend() ->
    mysend(<<"hello">>).

mysend(Message) ->
    %SocketOld = open(),
    {ok, Socket} = gen_udp:open(14999, [{broadcast, true}]),
    io:format("sender opened socket=~p~n",[Socket]),

    ok = gen_udp:send(Socket, {225,0,10,1}, 15000, Message),
    
    gen_udp:close(Socket).


open() ->
    PORT_NUM_TX_MULTI = 14999,
    GwIP = {0,0,0,0},
    MultiAddr = {225,0,10,1},
    {ok, Socket} = gen_udp:open(PORT_NUM_TX_MULTI,
                [
                    binary, 
                    {active, false},
                    {ip, GwIP},
                    %inet, 
                    %{multicast_ttl, 255},
                    %{multicast_loop, false},
                    {multicast_if, GwIP},
                    {add_membership, {MultiAddr, GwIP}},
                    {broadcast, true}
                ]),
    Socket.
