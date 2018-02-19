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
%    voteYes/0,
%    tellmi/2,
%    pongGGT/2,
%    calc_new_mi/2


go_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

init_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

init_receive_loop_1_test() ->
    InstanceVariables = {nameA, empty, empty},
    ThisPid = self(),
    TestPid = spawn(fun() ->
                        NewInstanceVariables = ggtprozess:init_receive_loop(InstanceVariables, empty),
                        ThisPid ! NewInstanceVariables
                    end),
    TestPid ! {setpm, 3},
    TestPid ! {setneighbors, ThisPid, ThisPid},

    receive
        Any -> ?assertEqual({nameA, 3, {ThisPid, ThisPid}}, Any)
        after 500 -> true = false
    end.

empty_instance_variables_exist_1_test() ->
    InstanceVariables = {nameA, empty, empty},
    ?assertEqual(true, ggtprozess:empty_instance_variables_exist(InstanceVariables)).

empty_instance_variables_exist_2_test() ->
    InstanceVariables = {nameA, full, full},
    ?assertEqual(false, ggtprozess:empty_instance_variables_exist(InstanceVariables)).


receive_loop_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

kill_1_test() ->
    ThisPid = self(),
    _TestPid = spawn(fun() -> ggtprozess:kill(nameA, ThisPid) end),
    receive
        Any -> {AbsenderPid, _} = Any,
               ?assertEqual({AbsenderPid, {unbind, nameA}}, Any),
               AbsenderPid ! ok
        after 2 -> true = false
    end.

calc_and_send_new_mi_1_test() ->
    Mi = 42,
    NewMi = 2,
    Y = 2,
    Neighbors = {self(), self()},
    NewMi = ggtprozess:calc_and_send_new_mi(Mi, Y, Neighbors),
    receive
        {sendy, NewMi} ->
            receive
                {sendy, NewMi} -> ok
            end
    end.

calc_and_send_new_mi_2_test() ->
    Mi = 2,
    NewMi = 2,
    Y = 42,
    Neighbors = {self(), self()},
    NewMi = ggtprozess:calc_and_send_new_mi(Mi, Y, Neighbors),
    receive
        Any -> io:fwrite("Sollte nichts bekommen, aber bekam: ~p", [Any]), true = false
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
    ggtprozess:send_new_mi(NewMi, {self(), self()}),
    receive
        {sendy, NewMi} ->
            receive
                {sendy, NewMi} -> ok
            end
    end.

voteYes_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

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