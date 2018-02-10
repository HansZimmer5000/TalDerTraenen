
-module(onenodenservertest).

-export([start/0]).

start() ->
        ClientPid = spawn(fun() -> 
                                receive
                                    Any -> io:fwrite(io_lib:format("Received: ~p \n", [Any]))
                                end
                            end),
        ServerPid = spawn(fun() -> server_loop(ClientPid) end).
        
client_loop() ->
    receive
        Any -> io:fwrite(io_lib:format("Received: ~p \n", [Any])),
                client_loop()
    end.

server_loop(ClientPid) ->
    ClientPid ! a,
    io:fwrite(io_lib:format("Send: a \n", [])).
    %server_loop(ClientPid).