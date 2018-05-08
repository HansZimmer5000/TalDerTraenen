-module(receiver).

-export([
    start/2,
    start/3,
    
    loop/2,
    listen_to_slot/2
    ]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(SLOTLENGTHMS, 40).

% --------------------------------------------------

start(CorePid, StationName) ->
    start(CorePid, StationName,{?NSNAME, ?NSNODE}).

start(CorePid, StationName,NsPid) ->
    Pid = spawn(fun() -> loop(CorePid, StationName) end),
    NsPid ! {enlist, Pid},
    Pid.

loop(CorePid, StationName) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} ->
            io:fwrite(io_lib:format("Missed Message: ~p in loop", [Message]));
        listentoslot -> 
            listen_to_slot(CorePid, StationName);
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in loop", [Any]))
    end,
    loop(CorePid, StationName).

listen_to_slot(CorePid, StationName) ->
    timer:send_after(?SLOTLENGTHMS, self(), stop_listening),
    {SlotMessages, ReceivedTimes} = listen([], []),
    ConvertedSlotMessages = messagehelper:convertReceivedMessagesFromByte(SlotMessages, ReceivedTimes),
    StationWasInvolved = stationWasInvolved(ConvertedSlotMessages, StationName),

    CorePid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}.

listen(SlotMessages, ReceivedTimes) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} -> 
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [erlang:timestamp() | ReceivedTimes],
            listen(NewSlotMessages, NewReceivedTimes);
        stop_listening ->
            {SlotMessages, ReceivedTimes};
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in listen_to_slot", [Any])),
            listen(SlotMessages, ReceivedTimes)
    end.

stationWasInvolved([], _StationName) ->
    false;
stationWasInvolved(ConvertedSlotMessages, StationName) ->
    [FirstConvertedSlotMessage | RestConvertedSlotMessages] = ConvertedSlotMessages,
    MessageStationName = messagehelper:getStationName(FirstConvertedSlotMessage),
    case MessageStationName of
        StationName ->
            true;
        _Any ->
            stationWasInvolved(RestConvertedSlotMessages, StationName)
    end.    

%-------------------------------------------------------------------

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.