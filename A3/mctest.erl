-module(mctest).

-export([
    start/0,
    stop/1
    ]).

-define(IP, {239,0,10,1}). %But should be 225.10.1.2
-define(PORT, 6376). %But should be 15000 + teamNumber
% The IP addresses 224.0.0.0 through 239.255.255.255 are multicast addresses.

open(Addr,Port) ->
   {ok,S} = gen_udp:open(Port,[{reuseaddr,true}, {ip,Addr}, {multicast_ttl,1}, {multicast_loop,false}, binary]),
   inet:setopts(S,[{add_membership,{Addr,{0,0,0,0}}}]),
   S.

close(S) -> gen_udp:close(S).

start() ->
   S=open(?IP, ?PORT),
   Pid=spawn(?MODULE,receiver,[]),
   gen_udp:controlling_process(S,Pid),
   {S,Pid}.

stop({S,Pid}) ->
   close(S),
   Pid ! stop.

receiver() ->
   receive
       {udp, _Socket, IP, InPortNo, Packet} ->
           io:format("~n~nFrom: ~p~nPort: ~p~nData: ~p~n",[IP,InPortNo,inet_dns:decode(Packet)]),
           receiver();
       stop -> true;
       AnythingElse -> io:format("RECEIVED: ~p~n",[AnythingElse]),
           receiver()
   end. 