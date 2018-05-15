-module(station).

-export([
    start/1
]).

start(Input) when length(Input) == 6 ->
    [InterfaceNameAtom, McastAddressAtom, ReceivePortAtom, StationClassAtom, UTCoffsetMsAtom, StationNumberAtom] = Input,
    %io:fwrite("Got all that stuff ~p", [Input]),

    StationType = atom_to_list(StationClassAtom),
    OffsetMs = erlang:list_to_integer(atom_to_list(UTCoffsetMsAtom)),
    StationNumber  = erlang:list_to_integer(lists:flatten(io_lib:format("~s", [StationNumberAtom]))),
    case StationNumber > 9 of
        true -> StationName = lists:flatten(io_lib:format("team 06-~p", [StationNumber]));
        false -> StationName = lists:flatten(io_lib:format("team 06-0~p", [StationNumber]))
    end,
    LogFile = io_lib:format("~s.log", [StationName]),
    
    Now = vsutil:getUTC(),
    WaitingTimeTillFirstFrame = round(2000 - Now rem 1000),
    timer:sleep(WaitingTimeTillFirstFrame),
    logge_status("Starte Station~p um ~p", [StationNumber, vsutil:getUTC()], LogFile),
    %logge_status("Starte Station~p mit ~p", [StationNumber, [StationName, OffsetMs, InterfaceNameAtom, McastAddressAtom, ReceivePortAtom, LogFile]], LogFile),
    core:start(StationType, StationName, OffsetMs, InterfaceNameAtom, McastAddressAtom, ReceivePortAtom, LogFile);

start([StationClassAtom, UTCoffsetMsAtom, StationNumberAtom]) ->
    StationType = atom_to_list(StationClassAtom),
    OffsetMs = erlang:list_to_integer(atom_to_list(UTCoffsetMsAtom)),
    case atom_to_list(StationNumberAtom) > 9 of
        true -> StationName = "team 06-" ++ atom_to_list(StationNumberAtom);
        false -> StationName = "team 06-0" ++ atom_to_list(StationNumberAtom)
    end,
    LogFile = io_lib:format("~s.log", [StationName]),

    logge_status("Starte Station~p mit ~p", [StationNumberAtom, [StationType, OffsetMs]], LogFile),
    core:start(StationType, StationName, OffsetMs, LogFile).

%------------------------------------------

logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p STATION ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).
