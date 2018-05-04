-module(receiver).

-export([start/1]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(SLOTLENGTHMS, 20).

start(CorePid) ->
    Pid = spawn(fun() -> loop(CorePid) end),
    {?NSNAME, ?NSNODE} ! {enlist, Pid},
    Pid.

loop(CorePid) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} ->
            io:fwrite(io_lib:format("Missed Message: ~p in loop", [Message]));
        listen_to_slot -> 
            timer:send_after(?SLOTLENGTHMS, self(), stop_listening),
            listen_to_slot(CorePid, [], []);
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in loop", [Any]))
    end,
    loop(CorePid).

listen_to_slot(CorePid, SlotMessages, ReceivedTimes) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} -> 
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [erlang:timestamp() | ReceivedTimes],
            listen_to_slot(CorePid, NewSlotMessages, NewReceivedTimes);
        stop_listening ->
            CorePid ! {slotMessages, SlotMessages, ReceivedTimes},
            loop(CorePid);
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in listen_to_slot", [Any])),
            listen_to_slot(CorePid, SlotMessages, ReceivedTimes)
    end.


hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.