-module(mctest).

-export([
    start/0,
    stop/1
    ]).

-define(IP, {239,0,10,2}). %But should be 225.10.1.2
-define(PORT, 6376). %But should be 15000 + teamNumber
% The IP addresses 224.0.0.0 through 239.255.255.255 are multicast addresses.
% Wohl die Sockets immer wieder schließen sonst wills nicht mehr?

%http://erlang.org/pipermail/erlang-questions/2014-May/079061.html
%{ok, SockSrc} = gen_udp:open(Port, [{reuseaddr,true}, {ip, Src}, {multicast_ttl, 1}, {multicast_loop, false}, binary]),
% inet:setopts(SockSrc, [{add_membership, {Mc, Src}}]);

% OR

%works in erl
%Opts = [ { active, true },
%           { ip, Src },
%           { multicast_loop, true },
%           { reuseaddr, true },
%           list
%         ],
%  { ok, RecvSocket } = gen_udp:open (Port, Opts),
%  inet:setopts(RecvSocket, [{ add_membership, { Mc, Src }}]),

open(Addr,Port) ->
    IP = {224,0,0,1},
    Pt = 3333,
    Opts = [    {active, true},
                {reuseaddr, true}, 
                %{ip, IP}, 
                {multicast_ttl, 1}, 
                {multicast_loop, false}, %false},
                {multicast_if, IP}, 
                inet,
                binary
            ],
    {ok, S} = gen_udp:open(Pt, Opts),
    inet:setopts(S, [{add_membership, {IP, {0, 0, 0, 0}}}]),
    S.

close(S) -> gen_udp:close(S).

start() ->
   S = open(?IP, ?PORT),
   Pid = spawn(fun() -> receiver() end),
   gen_udp:controlling_process(S, Pid),
   {S, Pid}.

stop({S, Pid}) ->
   close(S),
   Pid ! stop.

receiver() ->
   receive
       {udp, _Socket, IP, InPortNo, Packet} ->
           io:format("~n~nFrom: ~p~nPort: ~p~nData: ~p~n", [IP, InPortNo, inet_dns:decode(Packet)]),
           receiver();
       stop -> true;
       AnythingElse -> io:format("RECEIVED: ~p~n", [AnythingElse]),
           receiver()
   end. 

