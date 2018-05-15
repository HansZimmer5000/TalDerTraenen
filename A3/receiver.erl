-module(receiver).

-export([
    start/6,
    create_socket/3,
    
    loop/4,
    listen_to_slot/4,

    collision_happend/3,
    send_to_core/5
]).

-define(SLOTLENGTHMS, 40).


start(CorePid, StationName, InterfaceNameAtom, McastAddressAtom, ReceivePort,LogFile) ->
    Socket = create_socket(InterfaceNameAtom, McastAddressAtom, ReceivePort),
    Pid = spawn(fun() -> loop(CorePid, StationName, Socket, LogFile) end),
    logge_status("startet", LogFile),
    Pid.

create_socket(_InterfaceNameAtom, McastAddressAtom, ReceivePort) ->
        Interface = {0, 0, 0, 0},
        {ok, McastAddress} = inet_parse:address(atom_to_list(McastAddressAtom)),
        {ok, Socket} = gen_udp:open(ReceivePort, [
            {mode, binary},
            {reuseaddr, true},
            {ip, McastAddress},
            {multicast_ttl, 4},
            {multicast_loop, true},
            {broadcast, true},
            {add_membership, {McastAddress, Interface}},
            {active, false}]), %once = dann mit receive Any -> ... end holen
        Socket.
    
% --------------------------------------------------

loop(CorePid, StationName, Socket, LogFile) ->
    receive
        listentoslot -> 
            listen_to_slot(CorePid, StationName, Socket, LogFile);
        Any -> 
            logge_status("Missed: ~p in loop", [Any], LogFile)
    end,
    loop(CorePid, StationName, Socket, LogFile).

listen_to_slot(CorePid, StationName, Socket, LogFile) ->
    StartTime = vsutil:getUTC(),
    {SlotMessages, ReceivedTimes} = listen(39, [], [], Socket, LogFile),
    spawn(fun() ->
            ReceivedTime = vsutil:getUTC(),
            ConvertedSlotMessages = messagehelper:convert_received_messages_from_byte(SlotMessages, ReceivedTimes),
            {CollisionHappend, StationWasInvolved} = collision_happend(ConvertedSlotMessages, StationName, LogFile),
            send_to_core(ConvertedSlotMessages, CollisionHappend, StationWasInvolved, CorePid, LogFile),
            DoneTime = vsutil:getUTC(),
            TotalTime = DoneTime - StartTime,
            case TotalTime > 40 of
                    true ->
                        logge_status("Time Spend: ~p (Listening) ~p (Converting and Collision check) ~p (Total)", [ReceivedTime - StartTime, DoneTime - ReceivedTime, TotalTime], LogFile);
                    _ ->
                        nothing
            end
        end).


listen(RestTimeMilliSec, SlotMessages, ReceivedTimes, _Socket, _LogFile) when RestTimeMilliSec =< 0 ->
    {SlotMessages, ReceivedTimes};
listen(RestTimeMilliSec, SlotMessages, ReceivedTimes, Socket, LogFile) ->
    StartListeningAt = vsutil:getUTC(),

    case gen_udp:recv(Socket, 0, RestTimeMilliSec) of
        {ok, {_Address, _Port, Message}} ->
            NewSlotMessages = [Message | SlotMessages],
            NewReceivedTimes = [vsutil:getUTC() | ReceivedTimes];
        {error, _Reason} ->
            NewSlotMessages = SlotMessages,
            NewReceivedTimes = ReceivedTimes
            %logge_status("Got gen_udp:recv error: ~p", [Reason], LogFile)
    end,
    EndingListeningAt = vsutil:getUTC(),
    ElapsedTimeMilliSec = EndingListeningAt - StartListeningAt,
    NewRestTimeMilliSec = RestTimeMilliSec - ElapsedTimeMilliSec,
    listen(NewRestTimeMilliSec, NewSlotMessages, NewReceivedTimes, Socket, LogFile).

collision_happend(ConvertedSlotMessages, StationName, LogFile) ->
    %If a message is handled as received in slot X but was meant for slot Y (X <> Y) its a collision.
    case length(ConvertedSlotMessages) of
        0 -> {false, false};
        1 -> {false, false};
        _Any -> 
            StationWasInvolved = station_was_involved(ConvertedSlotMessages, StationName, LogFile),
            {true, StationWasInvolved}
    end.

send_to_core(ConvertedSlotMessages, CollisionHappend, StationWasInvolved, CorePid, LogFile) ->
    case CollisionHappend of
        true ->
            CorePid ! {slotmessages, [], StationWasInvolved},

            %TODO: make better logging for this, don't print all the messages just the necessary infos
            logge_status("(Involved: ~p) Collision detected in: ~p", [StationWasInvolved, ConvertedSlotMessages], LogFile);
        false ->
            CorePid ! {slotmessages, ConvertedSlotMessages, StationWasInvolved}
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

