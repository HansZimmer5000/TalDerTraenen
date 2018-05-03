-module(sender).

-export([start/0]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start() ->
    Pid = spawn(fun() -> loop({?NSNAME, ?NSNODE}) end),
    Pid.

loop(Nameservice) ->
    receive
        {send, Message} -> send(Nameservice, Message)
    end,
    loop(Nameservice).

send(Nameservice, Message) ->
    Nameservice ! {multicast, Message}.



hole_wert_aus_config_mit_key(Key) ->
        {ok, ConfigListe} = file:consult('nameservice.cfg'),
        {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
        Value.