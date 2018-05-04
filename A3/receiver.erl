-module(receiver).

-export([
    start/1,
    start/2,
    
    loop/1,
    listen_to_slot/1
    ]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(SLOTLENGTHMS, 20).

start(CorePid) ->
    start(CorePid, {?NSNAME, ?NSNODE}).

% --------------------------------------------------

start(CorePid, NsPid) ->
    Pid = spawn(fun() -> loop(CorePid) end),
    NsPid ! {enlist, Pid},
    Pid.

loop(CorePid) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} ->
            io:fwrite(io_lib:format("Missed Message: ~p in loop", [Message]));
        listentoslot -> 
            listen_to_slot(CorePid);
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in loop", [Any]))
    end,
    loop(CorePid).

listen_to_slot(CorePid) ->
    timer:send_after(?SLOTLENGTHMS, self(), stop_listening),
    listen(CorePid, [], []).

listen(CorePid, SlotMessages, ReceivedTimes) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} -> 
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [erlang:timestamp() | ReceivedTimes],
            listen(CorePid, NewSlotMessages, NewReceivedTimes);
        stop_listening ->
            CorePid ! {slotmessages, SlotMessages, ReceivedTimes},
            loop(CorePid);
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in listen_to_slot", [Any])),
            listen(CorePid, SlotMessages, ReceivedTimes)
    end.


%-------------------------------------------------------------------

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.