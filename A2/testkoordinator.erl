-module(testkoordinator).

-include_lib("eunit/include/eunit.hrl").


%    start/0,

%    wait_and_collect_ggtpro/2,
%    create_circle/2,
%    set_neighbors/4,
%    get_next_to_last_and_last_elem/1

%    receive_loop/1,
%    briefmi/3,
%    briefterm/4,
%    reset/0,
%    step/0,
%    calc/2,
%    prompt/0,
%    nudge/2,
%    toggle/0,
%    kill/0,

%    ggtpropid_exists/2,
%    get_ggtpropid/2




start_1_test() -> 
    ThisPid = self(),
    TestKoPid = spawn(fun() -> 
                    koordinator:start(ThisPid)
                end),
    timer:sleep(timer:seconds(1)),
    receive
        Any ->
            {TestKoPid, {bind, koordinator, _Node}} = Any,
            ?assertEqual(
                {registered_name, koordinator},
                process_info(TestKoPid, registered_name))
    end,
    exit(TestKoPid, kill),
    timer:sleep(timer:seconds(1)).

start_2_test() -> 
    ThisPid = self(),
    TestKoPid = spawn(fun() -> 
                    koordinator:start(ThisPid)
                end),
    timer:sleep(timer:seconds(1)),
    receive
        Any1 ->
            {TestKoPid, {bind, koordinator, _Node}} = Any1,
            TestKoPid ! ok
    end,
    TestKoPid ! {ThisPid, getsteeringval},
    receive
        Any2 ->
            io:fwrite("Wenn hier fail, speziell die gesetzten SteeringValues (hier und in koordinator.erl) pruefen!"),
            ?assertEqual({steeringval, 0, 0, 2, 5}, Any2)
    end,
    exit(TestKoPid, kill),
    timer:sleep(timer:seconds(1)).
            
wait_and_collect_ggtpro_1_test() -> 
    ?assertEqual([], koordinator:wait_and_collect_ggtpro([], 0)).

wait_and_collect_ggtpro_2_test() -> 
    ThisPid = self(),
    TestKoPid = spawn(fun() -> 
                    GGTProNameList = koordinator:wait_and_collect_ggtpro([], 2), 
                    ThisPid ! GGTProNameList
                end),
    TestKoPid ! {hello, nameA},
    TestKoPid ! {hello, nameB},
    receive
        Any -> ?assertEqual([nameB, nameA], Any)
    end.

create_circle_1_test() -> 
    ?assertError({badmatch, []}, koordinator:create_circle([], self())).

create_circle_2_test() -> 
    ?assertError({badmatch, [nameA]}, koordinator:create_circle([nameA], self())).

create_circle_3_test() -> 
    ThisPid = self(),
    TestPid = spawn(fun() -> 
            koordinator:create_circle([nameA, nameB, nameC], ThisPid)
        end),
    io:fwrite("a"),
    receive
        Any1 -> 
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any2 ->
            {setneighbors, nameC, nameB} = Any2
    end,
    receive
        Any3 -> 
            {TestPid, {lookup, nameC}} = Any3,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any4 ->
            {setneighbors, nameB, nameA} = Any4
    end,
    receive
        Any5 -> 
            {TestPid, {lookup, nameB}} = Any5,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any6 ->
            {setneighbors, nameA, nameC} = Any6
    end.

set_neighbors_1_test() -> 
    ThisPid = self(),
    TestPid = spawn(fun() ->
            koordinator:set_neighbors(nameA, nameC, nameB, ThisPid)
        end),
    receive
        Any1 -> 
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any2 ->
            {setneighbors, nameC, nameB} = Any2
    end.

get_next_to_last_and_last_elem_1_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([1,2])).

get_next_to_last_and_last_elem_2_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([3,4,5,6,1,2])).

receive_loop_1_test() -> 
    io:fwrite("Not yet implemented"),
    true = false.

briefmi_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.
briefterm_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

reset_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

step_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

calc_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

prompt_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

nudge_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ok = koordinator:nudge([nameA], ThisPid)
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! {pin, ThisPid}
    end,
        receive
        Any2 ->
            {TestPid, {lookup, nameA}} = Any2,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any3 ->
            {TestPid, pingGGT} = Any3,
            TestPid ! {pongGGT, nameA}
    end.

nudge_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ?assertError(
                            {badmatch, false}, 
                            koordinator:nudge([nameA], ThisPid))
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! not_found
    end.

toggle_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

kill_1_test() ->
    io:fwrite("Not yet implemented"),
    true = false.

ggtpropid_exists_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        Result = koordinator:ggtpropid_exists(nameA, ThisPid),
                        ThisPid ! Result
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any ->
            ?assert(Any)
    end.

ggtpropid_exists_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        Result = koordinator:ggtpropid_exists(nameA, ThisPid),
                        ThisPid ! Result
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! not_found
    end,
    receive
        Any ->
            ?assertEqual(false, Any)
    end.

get_ggtpropid_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        Result = koordinator:get_ggtpropid(nameA, ThisPid),
                        ThisPid ! Result
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! {pin, ThisPid}
    end,
    receive
        Any ->
            ?assertEqual(ThisPid, Any)
    end.

get_ggtpropid_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ?assertError({badmatch, false}, koordinator:get_ggtpropid(nameA, ThisPid))
                    end),
    receive
        Any1 ->
            {TestPid, {lookup, nameA}} = Any1,
            TestPid ! not_found
    end.