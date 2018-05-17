-module(inettest).

-export([test/0]).

test() ->
    {ok, IfList} = inet:getifaddrs(),
    go_through_iflist(IfList).

go_through_iflist([]) -> done;
go_through_iflist(IfList) ->
    [CurrentIf | RestIfs] = IfList,
    {CurrentIfName, CurrentIfOpts} = CurrentIf,
    io:fwrite("Looking for ~p: ", [CurrentIfName]),
    look_up_for_addr_tupel(CurrentIfOpts),
    go_through_iflist(RestIfs).

look_up_for_addr_tupel([]) -> done;
look_up_for_addr_tupel(IfOpts) ->
    [CurrentIfOpt | RestIfOpts] = IfOpts,
    {CurrentIfOptName, CurrentIfOptValue} = CurrentIfOpt,
    case CurrentIfOptValue of
        {A,B,C,D} when CurrentIfOptName == addr ->
            io:fwrite("~p~n", [{A,B,C,D}]);
        _ ->
            nothing
    end,
    look_up_for_addr_tupel(RestIfOpts).