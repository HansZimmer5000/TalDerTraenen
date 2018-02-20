-module(teststarter).

-include_lib("eunit/include/eunit.hrl").

%    go/1,

%    start_all_ggtprozesse/2,

%    create_ggtproname/2

go_1_test() ->
    GGTProName1 = 'ggt-1121',
    ThisPid = self(),
    StarterPid = spawn(
                    fun() ->
                        starter:go(1, ThisPid)
                    end),
    receive
        {StarterPid, getsteeringval} -> 
            StarterPid ! {steeringval, 0, 0, 2, 1}
    end,
    timer:sleep(timer:seconds(1)),
    Result = whereis(GGTProName1),
    io:fwrite("Ergebnis: ~p, ggf. Sleepzeit anpassen bei langsameren PCs", [Result]),
    ?assert(is_pid(Result)).


start_all_ggtprozesse_1_test() ->
    GGTProName1 = 'ggt-2121',
    starter:start_all_ggtprozesse(2, {empty, empty, empty, 1}),
    Result = whereis(GGTProName1),
    ?assert(is_pid(Result)).

create_ggtproname_1_test() -> 
    ?assertEqual('ggt-1123', starter:create_ggtproname(1, 3)).
