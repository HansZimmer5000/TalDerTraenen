-module(sender).

-export(
    [
        mysend/0, 
        mysend/1
    ]).

mysend() ->
    mysend(<<"hello">>).

mysend(Message) ->
    Socket = open(),

    ok = gen_udp:send(Socket, {225,0,10,1}, 6001, Message),
    
    gen_udp:close(Socket).


open() ->
    PORT_NUM_TX_MULTI = 6000,
    GwIP = {0,0,0,0},
    {ok, Socket} = gen_udp:open(PORT_NUM_TX_MULTI,
                [
                    binary, 
                    {active, false},
                    {ip, GwIP},
                    %inet, 
                    %{multicast_ttl, 255},
                    %{multicast_loop, false},
                    {multicast_if, GwIP}
                ]),
    io:format("sender opened socket=~p~n",[Socket]),
    Socket.
