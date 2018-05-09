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


start(CorePid, StationName) ->
    start(CorePid, StationName,{?NSNAME, ?NSNODE}).

start(CorePid, StationName,NsPid) ->
    Pid = spawn(fun() -> loop(CorePid, StationName) end),
    NsPid ! {enlist, Pid},
    Pid.

% --------------------------------------------------

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
    ConvertedSlotMessages = messagehelper:convert_received_messages_from_byte(SlotMessages, ReceivedTimes),
    {CollisionHappend, StationWasInvolved} = collision_happend(ConvertedSlotMessages, StationName),
    case CollisionHappend of
        true ->
            CorePid ! {slotmessages, [], StationWasInvolved};
        false ->
            CorePid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}
    end.

listen(SlotMessages, ReceivedTimes) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} -> 
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [vsutil:getUTC() | ReceivedTimes],
            listen(NewSlotMessages, NewReceivedTimes);
        stop_listening ->
            {SlotMessages, ReceivedTimes};
        Any -> 
            io:fwrite(io_lib:format("Got: ~p in listen_to_slot", [Any])),
            listen(SlotMessages, ReceivedTimes)
    end.

collision_happend(ConvertedSlotMessages, StationName) ->
        case length(ConvertedSlotMessages) of
            0 -> {true, false};
            1 -> {false, false};
            _Any -> {true, station_was_involved(ConvertedSlotMessages, StationName)}
        end.

station_was_involved([], _StationName) ->
    false;
station_was_involved(ConvertedSlotMessages, StationName) ->
    [FirstConvertedSlotMessage | RestConvertedSlotMessages] = ConvertedSlotMessages,
    MessageStationName = messagehelper:get_station_name(FirstConvertedSlotMessage),
    case MessageStationName of
        StationName ->
            true;
        _Any ->
            station_was_involved(RestConvertedSlotMessages, StationName)
    end.    

%-------------------------------------------------------------------

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.