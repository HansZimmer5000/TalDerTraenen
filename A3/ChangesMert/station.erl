-module(station).

-export([
    start/1
]).

start([StationTypeAtom, StationNameAtom]) ->
    StationNameRaw = atom_to_list(StationNameAtom),

    StationType = atom_to_list(StationTypeAtom),
    StationName = "team " ++ lists:sublist(StationNameRaw, 6,5),
    LogFile = io_lib:format("~s.log", [StationNameRaw]),
    io:fwrite("Starting as ~p and writing to Logfile: ~s\n", [StationName, LogFile]),
    core:start(StationType, StationName, LogFile).