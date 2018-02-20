-module(koordinator).

-export([
    start/0,

    wait_and_collect_ggtpro/2,
    create_circle/1,
    set_neighbors/3,

    receive_loop/1,

    get_next_to_last_and_last_elem/1
]).

-define(NSPID, {nameservice,'ns@HansZimmer-PC'}).
-define(KONAME, koordinator).
-define(ARBEITSZEIT, 0).
-define(TERMZEIT, 0).
-define(QUOATA, 2).
-define(GGTPROANZ, 5).

% WARNING! Ausgelegt nur für einen Starter!! (ein Recieveblock in start/0)
%1. start()
%2. register
%3. bind
%4. warte auf steeringval anfrage
%5. warte auf alle ggts (hallo)
%6. mache Kreis
%7. receive


start() ->

    ?GGTPROANZ >= 3,

    register(?KONAME, self()),
    ?NSPID ! {self(), {bind, ?KONAME, node()}},
    receive
        ok -> ok
    end,
    receive
        {AbsenderPid, getsteeringval} ->
            AbsenderPid ! {steeringval, ?ARBEITSZEIT, ?TERMZEIT, ?QUOATA, ?GGTPROANZ}
    end,
    GGTProNameList = wait_and_collect_ggtpro([], ?GGTPROANZ),
    receive_loop(GGTProNameList).

wait_and_collect_ggtpro(GGTProNameList, 0) -> 
    create_circle(GGTProNameList),
    GGTProNameList;
wait_and_collect_ggtpro(GGTProNameList, RestGGTProCount) ->
    receive
        {hello, GGTProName} -> 
            NewGGTProNameList = [GGTProName | GGTProNameList],
            NewRestGGTProCount = RestGGTProCount - 1,
            wait_and_collect_ggtpro(NewGGTProNameList, NewRestGGTProCount)
    end.

create_circle(GGTProNameList) ->
    [FirstGGTProName, SecondGGTProName | _RestGGTProNames] = GGTProNameList,
    [NextToLastGGTProName, LastGGTProName] = get_next_to_last_and_last_elem(GGTProNameList),
    set_neighbors(FirstGGTProName, LastGGTProName, SecondGGTProName),
    set_neighbors(LastGGTProName, NextToLastGGTProName, FirstGGTProName),
    create_circle_(GGTProNameList).

create_circle_([_NextToLastGGTProName, _LastGGTProName]) -> ok;
create_circle_([FirstGGTProName, SecondGGTProName, ThirdGGTProName | RestGGTProNames]) ->
    set_neighbors(SecondGGTProName, FirstGGTProName, ThirdGGTProName),
    create_circle_([SecondGGTProName, ThirdGGTProName | RestGGTProNames]).

set_neighbors(MiddleGGTProName, LeftGGTProName, RightGGTProName) ->
    ?NSPID ! {self(), {lookup, MiddleGGTProName}},
    receive
        {pin, MiddleGGTProPid} -> 
            MiddleGGTProPid ! {setneighbors, LeftGGTProName, RightGGTProName};
        not_found -> 
            io:fwrite("Circle kann nicht vervollständigt werden, ~p wurde beim nameservice nicht gefunden!", [MiddleGGTProName])
    end.



receive_loop(_GGTProNameList) ->
    receive
        {briefmi, {_GGTProName, _CMi, _CZeit}} -> none;
        {_AbsenderPid, briefterm, {_GGTProName, _CMi, _CZeit}} -> {sendy, LCMi};
        reset -> none;
        step -> none;
        {calc, _WggT} -> none;
        prompt -> none;
        nudge -> none;
        toggle -> none;
        kill -> none
    end.





get_next_to_last_and_last_elem([NextToLastElem, LastElem]) -> 
    [NextToLastElem, LastElem];
get_next_to_last_and_last_elem([_HeadElem | RestElems]) ->
    get_next_to_last_and_last_elem(RestElems).