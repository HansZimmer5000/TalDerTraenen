-module(station).

-export([
    start/1
]).

start([StationTypeAtom, StationNameAtom]) ->
    StationType = atom_to_list(StationTypeAtom),
    StationName = atom_to_list(StationNameAtom),
    LogFile = io_lib:format("~s.log", [StationName]),
    io:fwrite("Writing to Logfile: ~s\n", [LogFile]),
    core:start(StationType, StationName, LogFile).