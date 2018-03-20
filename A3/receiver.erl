-module(receiver).

-export([myreceive/1]).


myreceive(PORT_NUM_RX_MULTI) ->
    %Socket = open(PORT_NUM_RX_MULTI),
    %io:fwrite("got Port ~p", [inet:port(Socket)]),
    %inet:setopts(Socket, [{active, once}]),
    {ok, Socket} = gen_udp:open(PORT_NUM_RX_MULTI),
    io:fwrite("open"),
    receive
        {udp, Socket, Host, Port, Bin} ->
            gen_udp:send(Socket, Host, Port, Bin),
            io:fwrite("receiver~p received:~p~n",[PORT_NUM_RX_MULTI, Bin])
    end,

    gen_udp:close(Socket),

    myreceive(PORT_NUM_RX_MULTI).


open(PORT_NUM_RX_MULTI) ->
    GwIP = {0,0,0,0},
    MultiAddr = {225,0,10,1},
    {ok, Socket} = gen_udp:open(PORT_NUM_RX_MULTI,
                [
                    binary,
                    {active, false},
                    {ip, GwIP},
                    {multicast_if, GwIP},
                    %inet,
                    %{multicast_ttl, 255},
                    %{multicast_loop, false},
                    {add_membership, {MultiAddr, GwIP}},
                    {broadcast, true}
                ]),
    Socket.