-module(station).

-export([
    start/1
]).

start([StationTypeAtom]) ->
    StationType = atom_to_list(StationTypeAtom),
    core:start(StationType).