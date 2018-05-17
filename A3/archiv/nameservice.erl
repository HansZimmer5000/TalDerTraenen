-module(nameservice).

-export([
    start/0
]).

-define(NSNAME, nameservice).
-define(LOG_DATEI_NAME, "nameservice.log").

start() ->
    logge_status("nameservice gestartet"),
    register(?NSNAME, self()),
    spawn(fun() -> new_frame_loop() end),
    receive_loop([]).

new_frame_loop() ->
    DiffTime = 1000 - vsutil:getUTC() rem 1000,
    timer:sleep(DiffTime), %Its about 5 MS behind the second on average
    logge_status("--- New Frame at ~p ---", [vsutil:getUTC()]),
    new_frame_loop().

receive_loop(StationPids) ->
    receive
        {enlist, Pid} ->    logge_status("got enlist"),
                            NewStationPids = enlist(StationPids, Pid),
                            receive_loop(NewStationPids);
        {multicast, Message} ->     multicast(StationPids, Message),
                                    spawn(fun() -> print_message(Message) end),
                                    receive_loop(StationPids);
        Any ->  logge_status(io_lib:format("Got: ~p", [Any])),
                receive_loop(StationPids)
    end.

enlist(StationPids, NewPid) ->
    [NewPid | StationPids].


multicast([], _Message) -> done;
multicast(StationPids, Message) ->
    [CurrentStationPid | RestStationPids] = StationPids,
    CurrentStationPid !  {udp, empty, empty, empty, Message},
    multicast(RestStationPids, Message).
%-------------

print_message(Message) -> 
    [ConvertedMessage] = messagehelper:convert_received_messages_from_byte([Message],[vsutil:getUTC()]),
    StationType = messagehelper:get_station_type(ConvertedMessage),
    StationName = messagehelper:get_station_name(ConvertedMessage),
    Slotnumber = messagehelper:get_slotnumber(ConvertedMessage),
    SendTime = messagehelper:get_sendtime(ConvertedMessage),
    logge_status(
        "~p(~p) in Slot ~p at ~p", 
        [StationName, StationType, Slotnumber, SendTime]).


%--------------
logge_status(Text, Input) ->
    Inhalt = io_lib:format(Text, Input),
    logge_status(Inhalt).

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).