-module(ggtprozess).

-export([
    go/1,
    init/2,
    init_receive_loop/2,
    empty_instance_variables_exist/1,

    receive_loop/2,

    vote/3,
    kill/2,
    calc_and_send_new_mi/5,
    send_new_mi/4,
    vote_yes/1,
    start_vote/4,
    tellmi/2,
    pongGGT/2,
    calc_new_mi/2
]).

-define(CONFIG_FILENAME, "ggt.cfg").

-define(NSNODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(NSNAME, nameservice).

-define(KOORDINATORNAME, hole_wert_aus_config_mit_key(koordinatorname)).


go({GGTProName, ArbeitsZeit, TermZeit, Quota}) ->
    {KoPid, NsPid} = get_ko_and_ns_pid(GGTProName),
    go({GGTProName, ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});

go({GGTProName, ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    InstanceVariables = {GGTProName, empty, empty, false},
    GlobalVariables = {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid},

    GGTProPid = spawn(fun() -> init(InstanceVariables, GlobalVariables) end),
    true = register(GGTProName, GGTProPid),
    logge_status(GGTProName, lists:flatten(
                                io_lib:format("gestartet mit PID ~p",[GGTProPid]))),
    GGTProPid.

init({GGTProName, Mi, Neighbors, CalcIsDone}, {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    NsPid ! {self(), {rebind, GGTProName, node()}},
    receive
        ok -> logge_status(GGTProName, "registriert und bekannt beim nameservice")
    end,
    KoPid ! {hello, GGTProName},
    
    {GGTProName, FilledMi, FilledNeighbors, CalcIsDone} = init_receive_loop({GGTProName, Mi, Neighbors, CalcIsDone}, {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}),
    receive_loop({GGTProName, FilledMi, FilledNeighbors, empty, CalcIsDone}, 
                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}).

init_receive_loop({GGTProName, Mi, Neighbors, CalcIsDone}, GlobalVariables) ->
    receive
        {setneighbors, LeftN, RightN} ->  
            NewFirstInstanceVariables = {GGTProName, Mi, {LeftN, RightN}, CalcIsDone},
            case empty_instance_variables_exist(NewFirstInstanceVariables) of
                false -> NewFirstInstanceVariables;
                true -> 
                    logge_status(GGTProName, "init done"),
                    init_receive_loop(NewFirstInstanceVariables, GlobalVariables)
            end;
        {setpm, MiNeu} -> 
            NewFirstInstanceVariables = {GGTProName, MiNeu, Neighbors, CalcIsDone},
            case empty_instance_variables_exist(NewFirstInstanceVariables) of
                false -> NewFirstInstanceVariables;
                true ->  
                    logge_status(GGTProName, "init done"),
                    init_receive_loop(NewFirstInstanceVariables, GlobalVariables)
            end;
        kill -> 
            {_, _, _, NsPid, _} = GlobalVariables,
            kill(GGTProName, NsPid)
    end.

empty_instance_variables_exist(InstanceVariables) ->
    {GGTProName, Mi, Neighbors, CalcIsDone} = InstanceVariables,
    (GGTProName == empty) or (Mi == empty) or (Neighbors == empty) or (CalcIsDone == empty).

receive_loop({GGTProName, Mi, Neighbors, 0, OldCalcIsDone}, 
                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    case OldCalcIsDone of
        false -> 
            term(KoPid, GGTProName, Mi);
        _ -> donothing
    end,
    MissingCountForQuota = 0,
    CalcIsDone = true,
    receive
        {InitiatorPid, {vote, InitatorName}} -> logge_status_vote(GGTProName, InitatorName),
                                                vote(InitiatorPid, GGTProName, MissingCountForQuota),
                                                receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {sendy, Y} ->               logge_status(GGTProName, io_lib:format("Korrektur Y = ~p erhalten", [Y])),
                                    timer:sleep(timer:seconds(ArbeitsZeit)),
                                    NewMi = calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
                                    receive_loop({GGTProName, NewMi, Neighbors, empty, false},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {setpm, MiNeu} ->           logge_status(GGTProName, io_lib:format("Starte neue Berechnung mit Mi = ~p", [MiNeu])),
                                    receive_loop({GGTProName, MiNeu, Neighbors, empty, false}, 
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, tellmi} ->    tellmi(AbsenderPid, Mi),
                                    receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, pingGGT} ->   pongGGT(AbsenderPid, GGTProName),
                                    receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        kill -> kill(GGTProName, NsPid)
    end;
receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone}, 
                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    receive
        {InitiatorPid, {vote, InitatorName}} -> logge_status_vote(GGTProName, InitatorName),
                                                vote(InitiatorPid, GGTProName, MissingCountForQuota),
                                                receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                                {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {vote_yes, OtherGGTProName} ->  logge_status_vote_yes(GGTProName, OtherGGTProName),
                                        NewMissingCountForQuota = vote_yes(MissingCountForQuota),
                                        receive_loop({GGTProName, Mi, Neighbors, NewMissingCountForQuota, CalcIsDone},
                                                        {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {sendy, Y} ->               timer:sleep(timer:seconds(ArbeitsZeit)),
                                    NewMi = calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
                                    receive_loop({GGTProName, NewMi, Neighbors, empty, CalcIsDone},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, tellmi} ->    tellmi(AbsenderPid, Mi),
                                    receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        {AbsenderPid, pingGGT} ->   pongGGT(AbsenderPid, GGTProName),
                                    receive_loop({GGTProName, Mi, Neighbors, MissingCountForQuota, CalcIsDone},
                                                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});
        kill -> kill(GGTProName, NsPid)

        after timer:seconds(TermZeit) ->    NewMissingCountForQuota = start_vote(GGTProName, Mi, NsPid, Quota),
                                            receive_loop({GGTProName, Mi, Neighbors, NewMissingCountForQuota, CalcIsDone},
                                                            {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid})
    end.

term(KoPid, GGTProName, Mi) ->
    KoPid ! {self(), briefterm, {GGTProName, Mi, vsutil:now2string(erlang:timestamp())}},
    logge_status(GGTProName, "Genuegend Votes bekommen, briefterm gesendet").

vote(InitiatorPid, GGTProName, MissingCountForQuota) ->
    case MissingCountForQuota of
        empty -> donothing;
        _Any -> InitiatorPid ! {vote_yes, GGTProName}
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

vote_yes(MissingCountForQuota) ->
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
    logge_status(GGTProName, "ist heruntergefahren").

start_vote(GGTProName, Mi, NsPid, Quota) ->
    logge_status(GGTProName, lists:flatten(io_lib:format("mit ~p (Mi) wird vote gestartet", [Mi]))),
    NsPid ! {self(), {multicast, vote, GGTProName}},
    MissingCountForQuota = Quota,
    MissingCountForQuota.


get_ko_and_ns_pid(GGTProName) -> 
    net_adm:ping(?NSNODE),
    timer:sleep(timer:seconds(2)),
    case global:whereis_name(?NSNAME) of
        undefined -> 
            logge_status(GGTProName, "Nameservice global nicht gefunden, ggT faehrt runter"),
            timer:sleep(timer:seconds(5)),
            exit(kill);
        NsPid -> 
            NsPid ! {self(), {lookup, ?KOORDINATORNAME}},
            receive
                {pin, KoPid} -> 
                    {KoPid, NsPid};
                not_found -> 
                    logge_status(GGTProName, "Koordinator nicht im Nameservice bekannt, ggT faehrt runter"),
                    timer:sleep(timer:seconds(5)),
                    exit(kill)
            end
    end.

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status_vote(GGTProName1, GGTProName2) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("got vote from ~p", [GGTProName2]))).

logge_status_vote_yes(GGTProName1, GGTProName2) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("got vote_yes from ~p", [GGTProName2]))).

logge_status(GGTProName, Inhalt) ->
    LogDateiName = lists:flatten(io_lib:format("~p.log", [GGTProName])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s.\n", [GGTProName, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDateiName, LogNachricht).