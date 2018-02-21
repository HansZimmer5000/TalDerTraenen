-module(testkoordinator).

-include_lib("eunit/include/eunit.hrl").


%    start/0,

%    wait_and_collect_ggtpro/2,
%    create_circle/2,
%    set_neighbors/4,

%    receive_loop/1,

%    get_next_to_last_and_last_elem/1


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


receive_loop_1_test() -> 
    io:fwrite("Not yet implemented"),
    true = false.

get_next_to_last_and_last_elem_1_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([1,2])).

get_next_to_last_and_last_elem_2_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([3,4,5,6,1,2])).
