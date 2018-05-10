-module(receiver).

-export([
    start/3,
    start/4,
    
    loop/3,
    listen_to_slot/3
    ]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(SLOTLENGTHMS, 40).


start(CorePid, StationName, LogFile) ->
    start(CorePid, StationName, LogFile, {?NSNAME, ?NSNODE}).

start(CorePid, StationName, LogFile, NsPid) ->
    Pid = spawn(fun() -> loop(CorePid, StationName, LogFile) end),
    NsPid ! {enlist, Pid},
    logge_status("starte", LogFile),
    Pid.

% --------------------------------------------------

loop(CorePid, StationName, LogFile) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} ->
            logge_status("Missed Message: ~p in loop", [Message], LogFile);
        listentoslot -> 
            listen_to_slot(CorePid, StationName, LogFile);
        Any -> 
            logge_status("Got: ~p in loop", [Any], LogFile)
    end,
    loop(CorePid, StationName, LogFile).

listen_to_slot(CorePid, StationName, LogFile) ->
    timer:send_after(?SLOTLENGTHMS, self(), stop_listening),
    {SlotMessages, ReceivedTimes} = listen([], [], LogFile),
    ConvertedSlotMessages = messagehelper:convert_received_messages_from_byte(SlotMessages, ReceivedTimes),
    {CollisionHappend, StationWasInvolved} = collision_happend(ConvertedSlotMessages, StationName),
    case CollisionHappend of
        true ->
            CorePid ! {slotmessages, [], StationWasInvolved};
        false ->
            CorePid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}
    end.

listen(SlotMessages, ReceivedTimes, LogFile) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} -> 
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [vsutil:getUTC() | ReceivedTimes],
            listen(NewSlotMessages, NewReceivedTimes, LogFile);
        stop_listening ->
            {SlotMessages, ReceivedTimes};
        Any -> 
            logge_status("Got: ~p in listen_to_slot", [Any], LogFile),
            listen(SlotMessages, ReceivedTimes, LogFile)
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

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p - Recv ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

