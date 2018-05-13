-module(nameservice).

-export([
    start/0
]).

-define(NSNAME, nameservice).
-define(LOG_DATEI_NAME, "nameservice.log").

start() ->
    Starttime = vsutil:getUTC(),
	Pid = spawn(fun() -> receive_loop([], Starttime) end),	
    logge_status("nameservice gestartet ~p", [Pid]),	
    register (?NSNAME, Pid).

new_frame_loop(Starttime) ->
    DiffTime = 1000 - vsutil:getUTC() rem 1000,
    timer:sleep(DiffTime), %Its about 5 MS behind the second on average
    logge_status("--- New Frame at ~p ---", [vsutil:getUTC() - Starttime]),
    new_frame_loop(Starttime).

receive_loop(StationPids, Starttime) ->
    logge_status("loop gestartet"),	
    receive
        {enlist, Pid} ->    logge_status("got enlist"),
                            NewStationPids = enlist(StationPids, Pid),
                            receive_loop(NewStationPids, Starttime);
        {multicast, Message} ->     multicast(StationPids, Message),
                                    spawn(fun() -> print_message(Message, Starttime) end),
                                    receive_loop(StationPids, Starttime);
        Any ->  logge_status(io_lib:format("Got: ~p", [Any])),
                receive_loop(StationPids, Starttime)
    end.

enlist(StationPids, NewPid) ->
    [NewPid | StationPids].


multicast([], _Message) -> done;
multicast(StationPids, Message) ->
    [CurrentStationPid | RestStationPids] = StationPids,
    CurrentStationPid !  {udp, empty, empty, empty, Message},
    multicast(RestStationPids, Message).
%-------------

print_message(Message, Starttime) -> 
    [ConvertedMessage] = messagehelper:convert_received_messages_from_byte([Message],[vsutil:getUTC() - Starttime]),
    StationType = messagehelper:get_station_type(ConvertedMessage),
    StationName = messagehelper:get_station_name(ConvertedMessage),
    Slotnumber = messagehelper:get_slotnumber(ConvertedMessage),
    SendTime = messagehelper:get_sendtime(ConvertedMessage),
    logge_status(
        "~p(~p) in Slot ~p at ~p (~pms since Send)", 
        [StationName, StationType, Slotnumber, SendTime, SendTime - (vsutil:getUTC() - Starttime)]).


%--------------
logge_status(Text, Input) ->
    Inhalt = io_lib:format(Text, Input),
    logge_status(Inhalt).

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).