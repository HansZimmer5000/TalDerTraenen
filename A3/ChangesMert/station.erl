-module(station).

-export([
    start/1
]).

start([StationTypeAtom, StationNameAtom]) ->
    StationNameRaw = atom_to_list(StationNameAtom),

    StationType = atom_to_list(StationTypeAtom),
    StationName = "team " ++ lists:sublist(StationNameRaw, 6,5),
    LogFile = io_lib:format("~s.log", [StationNameRaw]),
	logge_status(StationType, LogFile),
    io:fwrite("Starting as ~p and writing to Logfile: ~s\n", [StationName, LogFile]),
    core:start(StationType, StationName, LogFile).
	
	
	
	logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p - Station ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).