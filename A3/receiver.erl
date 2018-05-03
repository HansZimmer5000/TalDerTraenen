-module(receiver).

-export([start/0]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start() ->
    Pid = spawn(fun() -> loop() end),
    {?NSNAME, ?NSNODE} ! {enlist, Pid},
    Pid.

loop() ->
    receive
        {multicast, _Message} -> dosomething
    end,
    loop().


hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.