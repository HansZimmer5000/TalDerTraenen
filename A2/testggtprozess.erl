-module(testggtprozess).

-include_lib("eunit/include/eunit.hrl").

%    go/1,
%    init/2,
%    init_receive_loop/2,
%    empty_instance_variables_exist/1,

%    receive_loop/2,

%    kill/2,
%    calc_and_send_new_mi/3,
%    send_new_mi/2,
%    vote_yes/0,
%    start_vote/4,
%    tellmi/2,
%    pongGGT/2,
%    calc_new_mi/2

-define(ARBEITSZEIT, 1).
-define(TERMZEIT, 5).
-define(QUOTA, 5).

go_1_test() ->
    ThisPid = self(),
    TestPid = ggtprozess:go({nameA, ?ARBEITSZEIT, ?TERMZEIT, ?QUOTA, ThisPid, ThisPid}),

    receive
        Any1 ->  ?assertEqual({TestPid, {rebind, nameA, node()}}, Any1),
                TestPid ! ok
    end,
    receive 
        Any2 -> ?assertEqual({hello, nameA}, Any2)
    end,

    TestPid ! {setpm, 3},
    TestPid ! {setneighbors, ThisPid, ThisPid},

    TestPid ! {ThisPid, tellmi},
    receive
        Any3 -> ?assertEqual({mi, 3}, Any3), ok
    end,

    TestPid ! kill,
    receive
        Any4 -> ?assertEqual({TestPid, {unbind, nameA}}, Any4),
                TestPid ! ok
    end.

init_1_test() ->
    ThisPid = self(),
    InstanceVariables = {nameA, empty, empty, false},
    GlobalVariables = {?ARBEITSZEIT, ?TERMZEIT, ?QUOTA, ThisPid, ThisPid},
    TestPid = spawn(fun() ->
                        ggtprozess:init(InstanceVariables, GlobalVariables)
                    end),
    receive
        Any1 ->  ?assertEqual({TestPid, {rebind, nameA, node()}}, Any1),
                TestPid ! ok
    end,
    receive 
        Any2 -> ?assertEqual({hello, nameA}, Any2)
    end,

    TestPid ! {setpm, 3},
    TestPid ! {setneighbors, ThisPid, ThisPid},

    TestPid ! {ThisPid, tellmi},
    receive
        Any3 -> ?assertEqual({mi, 3}, Any3), ok
    end,

    TestPid ! kill,
    receive
        Any4 -> ?assertEqual({TestPid, {unbind, nameA}}, Any4),
                TestPid ! ok
    end.

init_receive_loop_1_test() ->
    InstanceVariables = {nameA, empty, empty, false},
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        NewInstanceVariables = ggtprozess:init_receive_loop(InstanceVariables, empty),
                        ThisPid ! NewInstanceVariables
                    end),
    TestPid ! {setpm, 3},
    TestPid ! {setneighbors, ThisPid, ThisPid},

    receive
        Any -> ?assertEqual({nameA, 3, {ThisPid, ThisPid}, false}, Any)
    end.

empty_instance_variables_exist_1_test() ->
    InstanceVariables = {nameA, empty, empty, false},
    ?assert(ggtprozess:empty_instance_variables_exist(InstanceVariables)).

empty_instance_variables_exist_2_test() ->
    InstanceVariables = {nameA, full, full, false},
    ?assertNot(ggtprozess:empty_instance_variables_exist(InstanceVariables)).

empty_instance_variables_exist_3_test() ->
    InstanceVariables = {nameA, full, full, empty},
    ?assert(ggtprozess:empty_instance_variables_exist(InstanceVariables)).

receive_loop_1_test() ->
    ThisPid = self(),
    MissingCountForQuota = empty,
    InstanceVariables = {nameA, 3, {ThisPid, ThisPid}, MissingCountForQuota, false},
    GlobalVariables = {?ARBEITSZEIT, ?TERMZEIT, ?QUOTA, ThisPid, ThisPid},
    TestPid = spawn(fun() ->
                        ggtprozess:receive_loop(InstanceVariables, GlobalVariables)
                    end),
    TestPid ! {ThisPid, tellmi},
    receive
        Any1 -> ?assertEqual({mi, 3}, Any1), ok
    end,

    TestPid ! kill,
    receive
        Any2 -> ?assertEqual({TestPid, {unbind, nameA}}, Any2),
                TestPid ! ok
    end.

kill_1_test() ->
    ThisPid = self(),
    _TestPid = spawn(fun() -> ggtprozess:kill(nameA, ThisPid) end),
    receive
        Any -> {AbsenderPid, _} = Any,
               ?assertEqual({AbsenderPid, {unbind, nameA}}, Any),
               AbsenderPid ! ok
        after 2 -> throw("Not implemented yet")
    end.

calc_and_send_new_mi_1_test() ->
    Mi = 42,
    NewMi = 2,
    Y = 2,
    Neighbors = {self(), self()},
    GGTProName = nameA,
    KoPid = self(),
    NewMi = ggtprozess:calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
    receive
        Any1 -> ?assertEqual({sendy, NewMi}, Any1)
    end,
    receive
        Any2 -> ?assertEqual({sendy, NewMi}, Any2)
    end,
    receive
        Any3 -> {briefmi, {GGTProName, NewMi, _Timestamp}} = Any3
    end,
    receive 
        _Any4 -> ?assert(false)
        after 20 -> ok
    end.

calc_and_send_new_mi_2_test() ->
    Mi = 2,
    NewMi = 2,
    Y = 42,
    Neighbors = {self(), self()},
    GGTProName = nameA,
    KoPid = self(),
    NewMi = ggtprozess:calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
    receive
        Any -> 
            io:fwrite("Sollte nichts bekommen, aber bekam: ~p", [Any]), 
            throw("Not implemented yet")
        after 2 -> ok
    end.

calc_new_mi_1_test() ->
    ?assertEqual(2, ggtprozess:calc_new_mi(42,2)).

calc_new_mi_2_test() ->
    ?assertEqual(2, ggtprozess:calc_new_mi(2,42)).

calc_new_mi_3_test() ->
    ?assertEqual(208, ggtprozess:calc_new_mi(192400,704)).

send_new_mi_1_test() ->
    NewMi = 5,
    GGTProName = nameA,
    KoPid = self(),
    ggtprozess:send_new_mi(NewMi, {self(), self()}, GGTProName, KoPid),
    receive
        Any1 -> ?assertEqual({sendy, NewMi}, Any1)
    end,
    receive
        Any2 -> ?assertEqual({sendy, NewMi}, Any2)
    end,
    receive
        Any3 -> {briefmi, {GGTProName, NewMi, _Timestamp}} = Any3
    end,
    receive 
        _Any4 -> ?assert(false)
        after 20 -> ok
    end.

vote_yes_1_test() ->
    ?assertEqual(2, ggtprozess:vote_yes(3)).

start_vote_1_test() ->
    GGTProName = nameA,
    Mi = 3,
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                    MissingCountForQuota = ggtprozess:start_vote(GGTProName, Mi, ThisPid, ?QUOTA),
                    ThisPid ! MissingCountForQuota
                    end),

    receive
        Any1 -> ?assertEqual({TestPid, {multicast, vote, nameA}}, Any1)
    end,

    receive
        Any2 -> ?assertEqual(5, Any2)
    end.
    

tellmi_1_test() ->
    ggtprozess:tellmi(self(), 5),
    receive
        {mi, 5} -> ok
    end.

pongGGT_1_test() ->
    ggtprozess:pongGGT(self(), nameA),
    receive
        {pongGGT, nameA} -> ok
    end.