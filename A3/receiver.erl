-module(receiver).

-export([start/1]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start(CorePid) ->
    Pid = spawn(fun() -> loop(CorePid) end),
    {?NSNAME, ?NSNODE} ! {enlist, Pid},
    Pid.

loop(CorePid) ->
    receive
         {udp, _Socket0, _Ip0, _Port0, Message} -> CorePid ! Message;
        Any -> io:fwrite(io_lib:format("Got: ~p", [Any]))
    end,
    loop(CorePid).


hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.