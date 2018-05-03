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
        {multicast, Message} -> CorePid ! Message
    end,
    loop(CorePid).


hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.