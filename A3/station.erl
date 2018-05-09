-module(station).

-export([
    start/1
]).

start([StationTypeAtom, StationNameAtom]) ->
    StationType = atom_to_list(StationTypeAtom),
    StationName = atom_to_list(StationNameAtom),
    LogFile = io_lib:format("~p.log", [StationName]),
    core:start(StationType, StationName, LogFile).