-module(testkoordinator).

-include_lib("eunit/include/eunit.hrl").


%    start/0,

%    wait_for_starters/2,
%    wait_and_collect_ggtpro/2,
%    create_circle/2,
%    set_neighbors/4,
%    get_next_to_last_and_last_elem/1

%    calculation_receive_loop/1,
%    briefmi/3,
%    briefterm/4,
%    reset/2,
%    calc/3,
%    get_pms/2,
%    select_random_some_ggtprocesses/1,
%    send_pms_to_ggtprocesses/3,
%    send_ys_to_ggtprocesses/3,
%    get_first_n_elems_of_list/3,
%    send_message_to_processname/3,
%    prompt/0,
%    nudge/2,
%    toggle/0,
%    kill/2,
%    kill_all_ggtprocesses/2,

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
    kill_pid_and_clear_this_mailbox(TestKoPid).

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
    kill_pid_and_clear_this_mailbox(TestKoPid).

wait_for_starters_1_test() ->
    SteeringValues = {steeringval, 0, 0, 1, 5},
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                    StartersCount = koordinator:wait_for_starters(SteeringValues, 0), 
                    ThisPid ! StartersCount
                end),
    TestPid ! {ThisPid, getsteeringval},
    receive
        Any1 ->
            ?assertEqual(SteeringValues, Any1)
    end,
    TestPid ! {ThisPid, getsteeringval},
    receive
        Any2 ->
            ?assertEqual(SteeringValues, Any2)
    end,
    receive
        Any3 ->
            ?assertEqual(2, Any3)
    end,
    kill_pid_and_clear_this_mailbox(TestPid).


wait_and_collect_ggtpro_1_test() -> 
    ?assertEqual([], koordinator:wait_and_collect_ggtpro([], 0)).

wait_and_collect_ggtpro_2_test() -> 
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                    GGTProNameList = koordinator:wait_and_collect_ggtpro([], 2), 
                    ThisPid ! GGTProNameList
                end),
    TestPid ! {hello, nameA},
    TestPid ! {hello, nameB},
    receive
        Any -> ?assertEqual([nameB, nameA], Any)
    end,
    kill_pid_and_clear_this_mailbox(TestPid).

create_circle_1_test() -> 
    ?assertError({badmatch, []}, koordinator:create_circle([], self())).

create_circle_2_test() -> 
    ?assertError({badmatch, [nameA]}, koordinator:create_circle([nameA], self())).

create_circle_3_test() -> 
    ThisPid = self(),
    TestPid = spawn(fun() -> 
            koordinator:create_circle([nameA, nameB, nameC], ThisPid)
        end),

    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive
        Any2 ->
            {setneighbors, nameC, nameB} = Any2
    end,
    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},
    receive
        Any4 ->
            {setneighbors, nameB, nameA} = Any4
    end,
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive
        Any6 ->
            {setneighbors, nameA, nameC} = Any6
    end,
    kill_pid_and_clear_this_mailbox(TestPid).

set_neighbors_1_test() -> 
    ThisPid = self(),
    TestPid = spawn(fun() ->
            koordinator:set_neighbors(nameA, nameC, nameB, ThisPid)
        end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive
        Any2 ->
            {setneighbors, nameC, nameB} = Any2
    end,
    kill_pid_and_clear_this_mailbox(TestPid).

get_next_to_last_and_last_elem_1_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([1,2])).

get_next_to_last_and_last_elem_2_test() -> 
    ?assertEqual(
        [1,2], 
        koordinator:get_next_to_last_and_last_elem([3,4,5,6,1,2])).

calculation_receive_loop_1_test() -> 
    throw("Not yet implemented").

briefmi_1_test() ->
    LogNachricht = koordinator:briefmi(nameA, 3, empty),
    ["nameA", "meldet", "3(CMi)", "false(TermFlag)" | _Rest] = string:tokens(LogNachricht, " ").


briefterm_1_test() ->
    LogNachricht = koordinator:briefterm(self(), nameA, 3, empty),
    ["nameA", "meldet", "3(CMi)", "true(TermFlag)" | _Rest] = string:tokens(LogNachricht, " ").

reset_1_test() ->
    throw("Not yet implemented").

calc_1_test() ->
    ThisPid = self(),
    GGTList = [nameA, nameB, nameC],
    WggT = 15,
    TestPid = spawn(fun() -> 
                       koordinator:calc(WggT, GGTList, ThisPid)
                    end),

    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},

    receive
        Any1 -> {setpm, _Any1Pm} = Any1
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},

    receive
        Any2 -> {setpm, _Any2Pm} = Any2
    end,

    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},

    receive
        Any3 -> {setpm, _Any3Pm} = Any3
    end,

    receive Any4 -> {TestPid, {lookup, Any4Name}} = Any4 end,
    TestPid ! {pin, ThisPid},
    receive Any5 -> {TestPid, {lookup, Any5Name}} = Any5 end,
    ?assertEqual(Any4Name, Any5Name),
    ?assert(lists:member(Any4Name, GGTList)),
    TestPid ! {pin, ThisPid},

    receive Any6 -> {sendy, _Any6Pm} = Any6 end,

    receive Any7 -> {TestPid, {lookup, Any7Name}} = Any7 end,
    TestPid ! {pin, ThisPid},
    receive Any8 -> {TestPid, {lookup, Any8Name}} = Any8 end,
    ?assertEqual(Any7Name, Any8Name),
    ?assert(lists:member(Any7Name, GGTList)),
    TestPid ! {pin, ThisPid},

    receive Any9 -> {sendy, _Any9Pm} = Any9 end,

    kill_pid_and_clear_this_mailbox(TestPid),
    clear_mailbox().



get_pms_1_test() ->
    GGTProNameList = [nameA, nameB, nameC, nameD],
    PMList = koordinator:get_pms(16, GGTProNameList),
    ?assertEqual(
        4,
        length(PMList)
    ).

select_random_some_ggtprocesses_1_test() ->
    GGTProNameList = [nameA, nameB, nameC],
    Result = koordinator:select_random_some_ggtprocesses(GGTProNameList),
    ResultStr = util:list2string(Result),
    ResultTokens = lists:delete("\n", string:tokens(ResultStr, " ")),
    io:fwrite("~p", [ResultTokens]),

    ?assertEqual(2, length(ResultTokens)),

    [ResultElem1, ResultElem2] = Result,

    ?assertNotEqual(ResultElem1, ResultElem2),
    ?assert(lists:member(ResultElem1, GGTProNameList)),
    ?assert(lists:member(ResultElem2, GGTProNameList)).


send_pms_to_ggtprocesses_1_test() ->
    ThisPid = self(),
    GGTList = [nameA, nameB, nameC],
    Pms = [3,2,1],
    TestPid = spawn(fun() -> 
                       koordinator:send_pms_to_ggtprocesses(Pms, GGTList, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},

    receive
        Any1 -> ?assertEqual({setpm, 3}, Any1)
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},

    receive
        Any2 -> ?assertEqual({setpm, 2}, Any2)
    end,

    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameC),
    TestPid ! {pin, ThisPid},

    receive
        Any3 -> ?assertEqual({setpm, 1}, Any3)
    end.

send_ys_to_ggtprocesses_1_test() ->
    ThisPid = self(),
    GGTList = [nameA, nameB],
    Ys = [3,2,1],
    TestPid = spawn(fun() -> 
                       koordinator:send_ys_to_ggtprocesses(Ys, GGTList, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},

    receive
        Any1 -> ?assertEqual({sendy, 3}, Any1)
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},

    receive
        Any2 -> ?assertEqual({sendy, 2}, Any2)
    end.

get_first_n_elems_of_list_1_test() ->
    List = [1,2,3,4,5,6,7,8],
    ?assertEqual(
        [2,1],
        koordinator:get_first_n_elems_of_list(2, List, [])).

get_first_n_elems_of_list_2_test() ->
    List = [1,2,3,4,5,6,7,8],
    ?assertEqual(
        [],
        koordinator:get_first_n_elems_of_list(0, List, [])).

send_message_to_processname_1_test() ->
    Message = hallo,
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                       koordinator:send_message_to_processname(Message, nameA, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},

    receive
        Any -> ?assertEqual(Message, Any)
    end.

prompt_1_test() ->
    GGTProNameList = [nameA, nameB],
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                       koordinator:prompt(GGTProNameList, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},

    receive 
        Any1 -> 
            {TestPid, tellmi} = Any1,
            TestPid ! {mi, 3}
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},

    receive 
        Any2 -> 
            {TestPid, tellmi} = Any2,
            TestPid ! {mi, 4}
    end.


nudge_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ok = koordinator:nudge([nameA], ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive
        Any3 ->
            {TestPid, pingGGT} = Any3,
            TestPid ! {pongGGT, nameA}
    end.

nudge_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ?assertThrow(
                            ggtpronameUnkownForNs, 
                            koordinator:nudge([nameA], ThisPid))
                    end),
    receive_lookup(nameA),
    TestPid ! not_found.

toggle_1_test() ->
    throw("Not yet implemented").

kill_1_test() ->
    ProList = [nameA, nameB],
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        koordinator:kill(ProList, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive 
        Any1 -> ?assertEqual(kill, Any1) 
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive 
        Any2 -> ?assertEqual(kill, Any2) 
    end,

    receive
        Any3 -> 
            {TestPid, {unbind, koordinator}} = Any3,
            TestPid ! ok
    end,
    timer:sleep(500),

    ?assertEqual(undefined, process_info(TestPid, registered_name)).

kill_all_ggtprocesses_1_test() ->
    ProList = [nameA, nameB],
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        koordinator:kill_all_ggtprocesses(ProList, ThisPid)
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive 
        Any1 -> ?assertEqual(kill, Any1) 
    end,

    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive_lookup(nameB),
    TestPid ! {pin, ThisPid},
    receive 
        Any2 -> ?assertEqual(kill, Any2) 
    end,

    timer:sleep(500).
    

ggtpropid_exists_1_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        Result = koordinator:ggtpropid_exists(nameA, ThisPid),
                        ThisPid ! Result
                    end),
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
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
    receive_lookup(nameA),
    TestPid ! not_found,
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
    receive_lookup(nameA),
    TestPid ! {pin, ThisPid},
    receive
        Any ->
            ?assertEqual(ThisPid, Any)
    end.

get_ggtpropid_2_test() ->
    ThisPid = self(),
    TestPid = spawn(fun() -> 
                        ?assertThrow(
                            ggtpronameUnkownForNs,
                            koordinator:get_ggtpropid(nameA, ThisPid))
                    end),
    receive_lookup(nameA),
    TestPid ! not_found.
%-----------------

receive_lookup(Name) ->
    receive
        Any -> 
            {_Absender, {lookup, Name}} = Any
    end.

kill_pid_and_clear_this_mailbox(Pid) ->
    exit(Pid, kill),
    clear_mailbox().
clear_mailbox() ->
    receive
        _Any -> clear_mailbox()
        after 0 -> cleared
    end.

