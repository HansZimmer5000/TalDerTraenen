-module(receiver).

-export([myreceive/0]).


myreceive() ->
    Socket = open(),

    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, Port, Bin} ->
            gen_udp:send(Socket, Host, Port, Bin),
            io:fwrite("receiver received:~p~n",[Bin])
    end,

    gen_udp:close(Socket),

    myreceive().


open() ->
    PORT_NUM_RX_MULTI = 6001,
    GwIP = {0,0,0,0},
    MultiAddr = {225,0,10,1},
    {ok, Socket} = gen_udp:open(PORT_NUM_RX_MULTI,
                [
                    binary,
                    {active, false},
                    {multicast_if, GwIP},
                    %inet,
                    %{multicast_ttl, 255},
                    %{multicast_loop, false},
                    {add_membership, {MultiAddr, GwIP}}
                ]),
    io:fwrite("open"),
    Socket.