-module(receiver).

-export([
    start/6,
    create_socket/3,
    
    listen_loop/4,

    listen_to_slots_and_adjust_clock_and_slots/6,
    collision_happend/3
]).

-define(SLOTLENGTHMS, 40).

% -------------------- Init --------------------------
start(CorePid, ClockPid, InterfaceAddress, McastAddress, ReceivePort,LogFile) ->
    Socket = create_socket_klc(InterfaceAddress, McastAddress, ReceivePort),
    Pid = spawn(fun() -> listen_loop(CorePid, ClockPid, Socket, LogFile) end),
    logge_status("Listening to ~p:~p", [McastAddress, ReceivePort], LogFile),
    Pid.
    
% --------------------------------------------------

listen_loop(CorePid, ClockPid, Socket, LogFile) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {_Address, _Port, Message}} ->
            ClockPid ! {getcurrenttime, self()},
            receive
                {currenttime, CurrentTime} ->
                    CorePid ! {receivedmessage, Message, CurrentTime}
            end;
        {error, _Reason} ->
            nothing
    end,
    listen_loop(CorePid, ClockPid, Socket, LogFile).

% ---------- Exported Functions -------------
listen_to_slots_and_adjust_clock_and_slots(0, _ClockPid, _SlotFinderPid, CorePid, _StationName, _LogFile) ->
    CorePid ! donelistening;
listen_to_slots_and_adjust_clock_and_slots(RestSlotCount, ClockPid, SlotFinderPid, CorePid, StationName, LogFile) ->
    {ReceivedMessages, ReceivedTimes} = listen_to_slot(40, [],[], LogFile),
    spawn(fun() -> 
            %logge_status("Received ~p Messages this slot", [length(ReceivedMessages)], LogFile),
            case length(ReceivedMessages) of
                0 ->
                    CorePid ! {stationwasinvolved, false};
                _Any ->
                    ConvertedMessages = messagehelper:convert_received_messages_from_byte(ReceivedMessages, ReceivedTimes),
                    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedMessages, StationName, LogFile),
                    case CollisionHappend of
                        true -> 
                            CorePid ! {stationwasinvolved, StationWasInvolved};
                        false ->
                            ClockPid ! {adjust, ConvertedMessages},
                            SlotFinderPid ! {newmessages, ConvertedMessages},
                            CorePid ! {stationwasinvolved, StationWasInvolved}
                    end
            end
        end),
    %logge_status("Got SlotMessages at ~p", [vsutil:getUTC() - FrameStart], LogFile),
    listen_to_slots_and_adjust_clock_and_slots(RestSlotCount - 1, ClockPid, SlotFinderPid, CorePid, StationName, LogFile).

% ------------ Internal Functions ---------
create_socket_klc(InterfaceAddress, McastAddress, ReceivePort) ->
    vsutil:openRec(McastAddress, InterfaceAddress, ReceivePort).

create_socket(InterfaceAddress, McastAddress, ReceivePort) ->
        {ok, Socket} = gen_udp:open(ReceivePort, [
            {mode, binary},
            {reuseaddr, true},
            {ip, InterfaceAddress}, %may use Mcast
            {multicast_ttl, 1},
            {multicast_loop, true},
            {broadcast, true},
            {add_membership, {McastAddress, InterfaceAddress}},
            {active, false}]), %once = dann mit receive Any -> ... end holen
        Socket.
    
listen_to_slot(RestSlotTime, Messages, ReceivedTimes, _LogFile) when RestSlotTime =< 1 ->
    {Messages, ReceivedTimes};
listen_to_slot(RestSlotTime, Messages, ReceivedTimes, LogFile) ->
    StartTime = vsutil:getUTC(),
    %logge_status("Waiting for next receivedmessage", LogFile),
    receive
        {receivedmessage, Message, ReceivedTime} ->
            NewMessages = [Message | Messages],
            NewReceivedTimes = [ReceivedTime | ReceivedTimes],
            NewRestSlotTime = RestSlotTime - (vsutil:getUTC() - StartTime),
            listen_to_slot(NewRestSlotTime, NewMessages, NewReceivedTimes, LogFile)
        after RestSlotTime - 1 ->  
            %logge_status("Got no receivedmessage this slot", LogFile),
            {Messages, ReceivedTimes}
    end.

collision_happend(ConvertedSlotMessages, StationName, LogFile) ->
    %If a message is handled as received in slot X but was meant for slot Y (X <> Y) its a collision.
    case length(ConvertedSlotMessages) of
        0 -> {false, false};
        1 -> {false, false};
        _Any -> 
            StationWasInvolved = station_was_involved(ConvertedSlotMessages, StationName, LogFile),
            {true, StationWasInvolved}
    end.

station_was_involved([], StationName, LogFile) ->
    logge_status("Wasn't Involved: ~p", [StationName], LogFile),
    false;
station_was_involved(ConvertedSlotMessages, StationName, LogFile) ->
    [FirstConvertedSlotMessage | RestConvertedSlotMessages] = ConvertedSlotMessages,
    MessageStationName = messagehelper:get_station_name(FirstConvertedSlotMessage),
    case string:equal(MessageStationName, StationName) of
        true ->
            logge_status("Was Involved: ~p ~p", [StationName, messagehelper:get_station_name(FirstConvertedSlotMessage)], LogFile),
            true;
        false ->
            station_was_involved(RestConvertedSlotMessages, StationName, LogFile)
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

