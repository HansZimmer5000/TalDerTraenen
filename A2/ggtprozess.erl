-module(ggtprozess).

-export([
    go/1,
    init/2,
    init_receive_loop/2,
    empty_instance_variables_exist/1,

    receive_loop/2,

    vote_loop/2,
    vote/3,
    kill/2,
    calc_and_send_new_mi/5,
    send_new_mi/4,
    voteYes/1,
    start_vote/4,
    tellmi/2,
    pongGGT/2,
    calc_new_mi/2
]).

-define(CONFIG_FILENAME, 'ggtprozess.cfg').
-define(NSPID, hole_wert_aus_config_mit_key(nspid)).
-define(KOPID, hole_wert_aus_config_mit_key(kopid)).

go({GGTProName, ArbeitsZeit, TermZeit, Quota}) ->
    go({GGTProName, ArbeitsZeit, TermZeit, Quota, ?NSPID, ?KOPID});

go({GGTProName, ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    InstanceVariables = {GGTProName, empty, empty},
    GlobalVariables = {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid},

    GGTProPid = spawn(fun() -> init(InstanceVariables, GlobalVariables) end),
    true = register(GGTProName, GGTProPid),
    logge_status(GGTProName, lists:flatten(
                                io_lib:format("gestartet mit PID ~p",[GGTProPid]))),
    GGTProPid.

init({GGTProName, Mi, Neighbors}, {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    NsPid ! {self(), {rebind, GGTProName, node()}},
    receive
        ok -> logge_status(GGTProName, "registriert und bekannt beim nameservice")
    end,
    KoPid ! {hello, GGTProName},
    
    {GGTProName, FilledMi, FilledNeighbors} = init_receive_loop({GGTProName, Mi, Neighbors}, {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}),
    NewTimer = new_timer(TermZeit),
    receive_loop({GGTProName, FilledMi, FilledNeighbors, NewTimer, empty}, 
                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}).

init_receive_loop({GGTProName, Mi, Neighbors}, GlobalVariables) ->
    receive
        {setneighbors, LeftN, RightN} ->  
            logge_status(GGTProName, "got setneighbors"),
            NewFirstInstanceVariables = {GGTProName, Mi, {LeftN, RightN}},
            case empty_instance_variables_exist(NewFirstInstanceVariables) of
                false -> NewFirstInstanceVariables;
                true -> 
                    logge_status(GGTProName, "init done"),
                    init_receive_loop(NewFirstInstanceVariables, GlobalVariables)
            end;
        {setpm, MiNeu} -> 
            logge_status(GGTProName, "got setpm"),
            NewFirstInstanceVariables = {GGTProName, MiNeu, Neighbors},
            case empty_instance_variables_exist(NewFirstInstanceVariables) of
                false -> NewFirstInstanceVariables;
                true ->  
                    logge_status(GGTProName, "init done"),
                    init_receive_loop(NewFirstInstanceVariables, GlobalVariables)
            end
    end.

empty_instance_variables_exist(InstanceVariables) ->
    {GGTProName, Mi, Neighbors} = InstanceVariables,
    (GGTProName == empty) or (Mi == empty) or (Neighbors == empty).

receive_loop({GGTProName, Mi, _Neighbors, Timer, 0}, 
                {_ArbeitsZeit, _TermZeit, _Quota, NsPid, KoPid}) ->
    kill_timer(Timer),
    KoPid ! {self(), briefterm, {GGTProName, Mi, vsutil:now2string(erlang:timestamp())}},
    logge_status(GGTProName, "Genuegend Votes bekommen, briefterm gesendet, warte auf 'kill' im 'vote_loop'"),
    vote_loop(GGTProName, NsPid);
receive_loop({GGTProName, Mi, Neighbors, Timer, MissingCountForQuota}, 
                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    receive
        {InitiatorPid, {vote, InitatorName}} -> logge_status_vote(GGTProName, InitatorName),
                                                NewTimer = reset_timer(Timer, TermZeit),
                                                vote(InitiatorPid, GGTProName, MissingCountForQuota),
                                                receive_loop({GGTProName, Mi, Neighbors, NewTimer, MissingCountForQuota},
                                                                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {voteYes, OtherGGTProName} ->   logge_status_voteyes(GGTProName, OtherGGTProName),
                                        NewTimer = reset_timer(Timer, TermZeit),
                                        NewMissingCountForQuota = voteYes(MissingCountForQuota),
                                        receive_loop({GGTProName, Mi, Neighbors, NewTimer, NewMissingCountForQuota},
                                                        {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {sendy, Y} ->               logge_status(GGTProName, "got sendy"),
                                    NewTimer = reset_timer(Timer, TermZeit),
                                    timer:sleep(timer:seconds(ArbeitsZeit)),
                                    NewMi = calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
                                    receive_loop({GGTProName, NewMi, Neighbors, NewTimer, empty},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, tellmi} ->    logge_status(GGTProName, "got tellmi"),
                                    NewTimer = reset_timer(Timer, TermZeit),
                                    tellmi(AbsenderPid, Mi),
                                    receive_loop({GGTProName, Mi, Neighbors, NewTimer, MissingCountForQuota},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, pingGGT} ->   logge_status(GGTProName, "got pingGGT"),
                                    NewTimer = reset_timer(Timer, TermZeit),
                                    pongGGT(AbsenderPid, GGTProName),
                                    receive_loop({GGTProName, Mi, Neighbors, NewTimer, MissingCountForQuota},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        kill ->     logge_status(GGTProName, "got kill"),
                    kill_timer(Timer),
                    kill(GGTProName, NsPid);
        vote ->     logge_status(GGTProName, "got vote"),
                    NewTimer = reset_timer(Timer, TermZeit),
                    NewMissingCountForQuota = start_vote(GGTProName, Mi, NsPid, Quota),
                    receive_loop({GGTProName, Mi, Neighbors, NewTimer, NewMissingCountForQuota},
                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid})
    end.

vote_loop(GGTProName, NsPid) ->
    receive
        {InitiatorPid, {vote, InitatorName}} -> logge_status_vote(GGTProName, InitatorName),
                                                vote(InitiatorPid, GGTProName, 0),
                                                vote_loop(GGTProName, NsPid);
        kill ->     logge_status(GGTProName, "got kill im vote_loop"),
                    kill(GGTProName, NsPid)
    end.

vote(InitiatorPid, GGTProName, MissingCountForQuota) ->
    case MissingCountForQuota of
        empty -> donothing;
        _Any -> InitiatorPid ! {voteYes, GGTProName}
    end.

calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid) ->
    NewMi = calc_new_mi(Mi, Y),
    case NewMi of
        Mi ->   ok;
        _Any -> send_new_mi(NewMi, Neighbors, GGTProName, KoPid)
    end,
    NewMi.

calc_new_mi(Mi, Y) -> 
    case Mi > Y of
        true -> NewMi = ((Mi - 1) rem Y) + 1;
        false -> NewMi = Mi
    end,
    NewMi.

send_new_mi(NewMi, {LeftN, RightN}, GGTProName, KoPid) ->
    LeftN ! {sendy, NewMi},
    RightN ! {sendy, NewMi},
    KoPid ! {briefmi, {GGTProName, NewMi, vsutil:now2string(erlang:timestamp())}}.

voteYes(MissingCountForQuota) ->
    case MissingCountForQuota of
        empty -> NewMissingCountForQuota = empty;
        _Any -> NewMissingCountForQuota = MissingCountForQuota - 1
    end,
    NewMissingCountForQuota.

tellmi(ReceiverPid, Mi) ->
    ReceiverPid ! {mi, Mi}.

pongGGT(ReceiverPid, GGTProName) ->
    ReceiverPid ! {pongGGT, GGTProName}.


kill(GGTProName, NsPid) ->
    NsPid ! {self(), {unbind, GGTProName}},
    receive
        ok -> ok
    end,

    case whereis(GGTProName) of
        undefined -> ok; %Only for Test purposes! Because since its in the same process the name is always registered during normal run until unregistered here.
        _Any -> unregister(GGTProName)
    end,
    ok.

start_vote(GGTProName, Mi, NsPid, Quota) ->
    logge_status(GGTProName, lists:flatten(io_lib:format("mit ~p (Mi) wird vote gestartet", [Mi]))),
    NsPid ! {self(), {multicast, vote, GGTProName}},
    MissingCountForQuota = Quota - 1, % '-1' because, this GGT-Process said 'yes' by starting the vote
    MissingCountForQuota.

reset_timer(OldTimer, SendAfterXSeconds) ->
    kill_timer(OldTimer),
    NewTimer = new_timer(timer:seconds(SendAfterXSeconds)),
    NewTimer.

kill_timer(OldTimer) ->
    timer:cancel(OldTimer).

new_timer(SendAfterXSeconds) ->
    NewTimer = timer:send_after(timer:seconds(SendAfterXSeconds), vote),
    NewTimer.

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status_vote(GGTProName1, GGTProName2) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("got vote from ~p", [GGTProName2]))).

logge_status_voteyes(GGTProName1, GGTProName2) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("got voteYes from ~p", [GGTProName2]))).

logge_status(GGTProName, Inhalt) ->
    LogDateiName = lists:flatten(io_lib:format("~p.log", [GGTProName])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s.\n", [GGTProName, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDateiName, LogNachricht).