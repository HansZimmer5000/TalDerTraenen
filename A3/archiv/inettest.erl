-module(inettest).

-export([test/1]).

test(InterfaceName) ->
    find_ipv4_addr_of_interface(InterfaceName).

find_ipv4_addr_of_interface(InterfaceName) ->
    {ok, IfList} = inet:getifaddrs(),
    case go_through_iflist(InterfaceName, IfList) of
        not_found -> 
            io:fwrite("No IfListEntry for InterfaceName~n");
        Addr -> 
            io:fwrite("Found Addr: ~p~n", [Addr])
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