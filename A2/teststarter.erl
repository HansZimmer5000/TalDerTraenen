-module(teststarter).

-include_lib("eunit/include/eunit.hrl").

%    go/1,

%    start_all_ggtprozesse/2,

%    create_ggtproname/2

go_1_test() ->
    GGTProName1 = 'ggt-1261',
    ThisPid = self(),
    register(nameservice, ThisPid),
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
    unregister(nameservice),
    ?assert(is_pid(Result)).


start_all_ggtprozesse_1_test() ->
    GGTProName1 = 'ggt-2261',
    register(nameservice, self()),
    starter:start_all_ggtprozesse(2, {empty, empty, empty, 1}),
    Result = whereis(GGTProName1),
    unregister(nameservice),
    ?assert(is_pid(Result)).

create_ggtproname_1_test() -> 
    ?assertEqual('ggt-1263', starter:create_ggtproname(1, 3)).
