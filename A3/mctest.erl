-module(mctest).

-export([start/0, client/1, open/2]).

-define(IP, {225,0,10,1}).
-define(PORT, 6300).

start() ->
    %spawn(fun() -> server(0) end).
    {ok, Socket} = open(?PORT, ?IP),
    spawn(fun() -> server(Socket) end),
    spawn(fun() -> client(Socket) end),
    server(Socket).


open(Port, Ip) ->
    gen_udp:open(
        Port,
        [
            binary,
            {multicast_if, Ip},
            {add_membership, {Ip, {0,0,0,0}}},
            %{broadcast, true},
            {active, false}
        ]
    ).

server(Socket) ->
    %{ok, Socket} = open(?PORT + Offset, ?IP),
    io:format("server opened socket:~p~n",[Socket]),
    myreceive(Socket).

myreceive(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("server received:~p~n",[Bin]),
            gen_udp:send(Socket, Host, Port, Bin),
            myreceive(Socket)
    end.

% Client code
client(Socket) ->
    N = <<"hallo">>,
    %{ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, ?IP, ?PORT, N),
    Length = 1000,
    {ok, {_Addr, _Port, Packet}} = gen_udp:recv(Socket, Length),
    io:fwrite("client received:~p~n",[Packet]).
    %gen_udp:close(Socket).







loopalt(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {udp, Socket, Host, Port, Bin} ->
            io:format("server received:~p~n",[Bin]),
            gen_udp:send(Socket, Host, Port, Bin),
            loopalt(Socket)
    end.

myreceivealt(Socket) ->
    Length = 1000,
    {ok, {_Addr, _Port, Packet}} = gen_udp:recv(Socket, Length),
    io:fwrite("server received:~p~n",[Packet]),
    myreceivealt(Socket).