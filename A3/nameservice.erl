-module(nameservice).

-export([
    start/0
]).

-define(NSNAME, nameservice).
-define(LOG_DATEI_NAME, "nameservice.log").

start() ->
    logge_status("nameservice gestartet"),
    register(?NSNAME, self()),
    receive_loop([]).

receive_loop(StationPids) ->
    receive
        {enlist, Pid} ->    logge_status("got enlist"),
                            NewStationPids = enlist(StationPids, Pid),
                            receive_loop(NewStationPids);
        {multicast, Message} ->     logge_status("got multicast"),
                                    multicast(StationPids, Message),
                                    receive_loop(StationPids);
        Any ->  logge_status(io_lib:format("Got: ~p", [Any])),
                receive_loop(StationPids)
    end.

enlist(StationPids, NewPid) ->
    [NewPid | StationPids].


multicast([], _Message) -> done;
multicast(StationPids, Message) ->
    [CurrentStationPid | RestStationPids] = StationPids,
    CurrentStationPid ! {multicast, Message},
    multicast(RestStationPids, Message).


logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).