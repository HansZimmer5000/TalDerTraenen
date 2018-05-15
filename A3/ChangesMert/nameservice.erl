-module(nameservice).

-export([
    start/0
]).

-define(NSNAME, nameservice).
-define(LOG_DATEI_NAME, "nameservice.log").

start() ->
    Starttime = vsutil:getUTC(),
	Pid = spawn(fun() -> receive_loop([]) end),	
    logge_status("nameservice gestartet ~p", [Pid]),
	logge_status(" ~p", [erlang:node()]),	
    true = register (?NSNAME, Pid).

receive_loop(StationPids) ->
    receive
        {enlist, Pid} ->    logge_status("got enlist"),
                            NewStationPids = enlist(StationPids, Pid),
                            receive_loop(NewStationPids),
							Pid ! ok;
							
        {multicast, Message} ->     multicast(StationPids, Message),
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

%--------------
logge_status(Text, Input) ->
    Inhalt = io_lib:format(Text, Input),
    logge_status(Inhalt).

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).