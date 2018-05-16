-module(receiver).

-export([
    start/6,
    create_socket/3,
    
    listen_loop/4,

    collision_happend/3
]).

-define(SLOTLENGTHMS, 40).


start(CorePid, ClockPid, _InterfaceNameAtom, McastAddressAtom, ReceivePort,LogFile) ->
       {ok, McastAddress} = inet_parse:address(atom_to_list(McastAddressAtom)),
    Socket = vsutil:openRec(McastAddress, {172,16,1,2}, ReceivePort),%create_socket({172,16,1,2}, McastAddressAtom, ReceivePort),
    Pid = spawn(fun() -> listen_loop(CorePid, ClockPid, Socket, LogFile) end),
    logge_status("Listening to ~p:~p", [McastAddressAtom, ReceivePort], LogFile),
    Pid.

create_socket(_InterfaceNameAtom, McastAddressAtom, ReceivePort) ->
	Interface = {172,16,1,2},
        {ok, McastAddress} = inet_parse:address(atom_to_list(McastAddressAtom)),
        {ok, Socket} = gen_udp:open(ReceivePort, [
            {mode, binary},
            {reuseaddr, true},
            {ip, Interface}, %may use Mcast
            {multicast_ttl, 1},
            {multicast_loop, true},
            {broadcast, true},
            {add_membership, {McastAddress, Interface}},
            {active, false}]), %once = dann mit receive Any -> ... end holen
        Socket.
    
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

