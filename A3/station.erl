-module(station).

-export([
    start/1
]).

start(Input) when length(Input) > 2->
    io:fwrite("~p", [Input]);

start([StationTypeAtom, StationNameAtom]) ->
    StationNameRaw = atom_to_list(StationNameAtom),

    StationType = atom_to_list(StationTypeAtom),
    StationName = "team " ++ lists:sublist(StationNameRaw, 6,5),
    LogFile = io_lib:format("~s.log", [StationNameRaw]),
    
    logge_status("Starting as ~p and writing to Logfile: ~s", [StationName, LogFile], LogFile),
    core:start(StationType, StationName, LogFile).

%------------------------------------------

logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p STATION ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).
