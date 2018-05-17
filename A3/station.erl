-module(station).

-export([
    start/1
]).

start(Input) when length(Input) == 6 ->
    [InterfaceNameAtom, McastAddressAtom, ReceivePortAtom, StationClassAtom, UTCoffsetMsAtom, StationNumberAtom] = Input,
    
    StationType = atom_to_list(StationClassAtom),
    StationName = create_station_name(StationNumberAtom),
    OffsetMs = erlang:list_to_integer(atom_to_list(UTCoffsetMsAtom)),
    LogFile = io_lib:format("~s.log", [StationName]),

    InterfaceAddress = find_ipv4_addr_of_interface(atom_to_list(InterfaceNameAtom), LogFile),
    {ok, McastAddress} = inet_parse:address(atom_to_list(McastAddressAtom)),

    Now = vsutil:getUTC(),
    WaitingTimeTillFirstFrame = 2000 - Now rem 1000,
    timer:sleep(WaitingTimeTillFirstFrame),
    logge_status(
        "Starte ~p um ~p mit ~p", 
        [StationName, vsutil:getUTC(), 
            [StationType, OffsetMs, InterfaceAddress, McastAddress, ReceivePortAtom, LogFile]], 
        LogFile).
    %core:start(StationType, StationName, OffsetMs, InterfaceAddress, McastAddress, ReceivePortAtom, LogFile).

%------------------------------------------

find_ipv4_addr_of_interface(InterfaceName, LogFile) ->
    DefaultAddr = {0,0,0,0},
    {ok, IfList} = inet:getifaddrs(),
    case go_through_iflist(InterfaceName, IfList) of
        not_found -> 
            logge_status("No IfListEntry for InterfaceName, using Standard", LogFile),
            DefaultAddr;
        Addr -> 
            logge_status("Found Addr: ~p~n", [Addr], LogFile),
            Addr
    end.

go_through_iflist(_Name, []) -> not_found;
go_through_iflist(InterfaceName, IfList) ->
    [CurrentIf | RestIfs] = IfList,
    {CurrentIfName, CurrentIfOpts} = CurrentIf,
    case CurrentIfName == InterfaceName of
        true ->
            look_up_for_addr_tupel(CurrentIfOpts);
        false ->
            go_through_iflist(InterfaceName, RestIfs)
    end.

look_up_for_addr_tupel([]) -> not_found;
look_up_for_addr_tupel(IfOpts) ->
    [CurrentIfOpt | RestIfOpts] = IfOpts,
    {CurrentIfOptName, CurrentIfOptValue} = CurrentIfOpt,
    case CurrentIfOptValue of
        {A,B,C,D} when CurrentIfOptName == addr ->
            {A,B,C,D};
        _ ->
            look_up_for_addr_tupel(RestIfOpts)
    end.

create_station_name(StationNumberAtom) -> 
    StationNumber  = erlang:list_to_integer(lists:flatten(io_lib:format("~s", [StationNumberAtom]))),
    case StationNumber > 9 of
        true -> StationName = lists:flatten(io_lib:format("team 06-~p", [StationNumber]));
        false -> StationName = lists:flatten(io_lib:format("team 06-0~p", [StationNumber]))
    end,
    StationName.

%--------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p STATION ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).
