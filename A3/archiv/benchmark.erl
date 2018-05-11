-module(benchmark).

-export([
    start/0
]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(BENCHCOUNT, 100).
-define(SENDCOUNT, 100).

start() ->
    NsIp = {?NSNAME, ?NSNODE},
    %TODO: enlist each benchmark otherwise just one gets
    NsIp ! {enlist, self()},

    io:fwrite("started with nsip = ~p\n", [NsIp]),

    Message = {{"A","team 06-02","-1234567890123",1,0},0},
    ByteMessage = messagehelper:convert_message_to_byte(Message),
    start_n_benchmarks(?BENCHCOUNT, NsIp, ByteMessage).

start_n_benchmarks(0,_,_) ->
    io:fwrite("All Benchmarks started\n");
start_n_benchmarks(RestCount, NsIp, ByteMessage) ->
    spawn(fun() -> 
            NsIp ! {enlist, self()},
            start(NsIp, ?SENDCOUNT, 0, 0, ByteMessage) 
        end),
    start_n_benchmarks(RestCount - 1, NsIp, ByteMessage).

start(_NsIp, 0, Total, Count, _ByteMessage) -> 
    io:fwrite("Average: ~p\n", [Total / Count]);
start(NsIp, RestCount, Total, Count, ByteMessage) ->
    timer:sleep(timer:seconds(1)),
    NeededTime = benchmark(NsIp, ByteMessage),
    start(NsIp, RestCount - 1, Total + NeededTime, Count + 1, ByteMessage).

benchmark(NsIp, Message) -> 
    StartTime = vsutil:getUTC(),
    NsIp ! {multicast, Message},
    receive
        {udp, empty, empty, empty, Message} ->
            EndTime = vsutil:getUTC(),
            %io:fwrite("Time needed: ~p\n", [EndTime - StartTime]),
            EndTime - StartTime
    end.



hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.
