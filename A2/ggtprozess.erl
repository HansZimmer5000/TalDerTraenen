-module(ggtprozess).

-export([
    go/1,
    init/2,
    init_receive_loop/2,
    empty_instance_variables_exist/1,

    receive_loop/2,

    kill/2,
    calc_and_send_new_mi/3,
    send_new_mi/2,
    voteYes/0,
    tellmi/2,
    pongGGT/2,
    calc_new_mi/2
]).

-define(CONFIG_FILENAME, 'ggtprozess.cfg').
-define(NSPID, hole_wert_aus_config_mit_key(nspid)).
-define(KOPID, hole_wert_aus_config_mit_key(kopid)).
-define(LOG_DATEI_NAME, 'ggtprozess.log').

go({GGTProName, ArbeitsZeit, TermZeit, Quota}) ->
    go({GGTProName, ArbeitsZeit, TermZeit, Quota, ?NSPID, ?KOPID});

go({GGTProName, ArbeitsZeit, TermZeit, Quota, NSPid, KOPid}) ->
    InstanceVariables = {GGTProName, empty, empty},
    GlobalVariables = {ArbeitsZeit, TermZeit, Quota, NSPid, KOPid},

    GGTProPid = spawn(fun() -> init(InstanceVariables, GlobalVariables) end),
    register(GGTProName, GGTProPid),
    NSPid ! {GGTProPid, {rebind, GGTProName, node()}},
    GGTProPid.

init(InstanceVariables, GlobalVariables) ->
    receive
        ok -> ok
    end,
    FilledInstanceVariables = init_receive_loop(InstanceVariables, GlobalVariables),
    receive_loop(FilledInstanceVariables, GlobalVariables).

init_receive_loop({GGTProName, Mi, Neighbors}, GlobalVariables) ->
    receive
        {setneighbors, LeftN, RightN} ->  
            NewInstanceVariables = {GGTProName, Mi, {LeftN, RightN}},
            case empty_instance_variables_exist(NewInstanceVariables) of
                false -> NewInstanceVariables;
                true -> io:fwrite("nr"), init_receive_loop(NewInstanceVariables, GlobalVariables)
            end;
        {setpm, MiNeu} -> 
            NewInstanceVariables = {GGTProName, MiNeu, Neighbors},
            case empty_instance_variables_exist(NewInstanceVariables) of
                false -> NewInstanceVariables;
                true ->  init_receive_loop(NewInstanceVariables, GlobalVariables)
            end
    end.

empty_instance_variables_exist(InstanceVariables) ->
    {GGTProName, Mi, Neighbors} = InstanceVariables,
    (GGTProName == empty) or (Mi == empty) or (Neighbors == empty).

receive_loop({GGTProName, Mi, Neighbors}, 
            {ArbeitsZeit, TermZeit, Quota, NSPID, KOPID}) ->
    receive
        kill ->     logge_status(GGTProName, "got kill"),
                    kill(GGTProName, NSPID);
        {sendy, Y} ->               logge_status(GGTProName, "got sendy"),
                                    NewMi = calc_and_send_new_mi(Mi, Y, Neighbors),
                                    receive_loop({GGTProName, NewMi, Neighbors},
                                                    {ArbeitsZeit, TermZeit, Quota, NSPID, KOPID});
        {voteYes, GGTProName} ->    logge_status(GGTProName, "got voteYes"),
                                    voteYes(),
                                    receive_loop({GGTProName, Mi, Neighbors},
                                                 {ArbeitsZeit, TermZeit, Quota, NSPID, KOPID});
        {AbsenderPid, tellmi} ->    logge_status(GGTProName, "got tellmi"),
                                    tellmi(AbsenderPid, Mi),
                                    receive_loop({GGTProName, Mi, Neighbors},
                                                {ArbeitsZeit, TermZeit, Quota, NSPID, KOPID});
        {AbsenderPid, pingGGT} ->   logge_status(GGTProName, "got pingGGT"),
                                    pongGGT(AbsenderPid, GGTProName),
                                    receive_loop({GGTProName, Mi, Neighbors},
                                                 {ArbeitsZeit, TermZeit, Quota, NSPID, KOPID})
    end.

kill(GGTProName, NSPID) ->
    NSPID ! {self(), {unbind, GGTProName}},
    receive
        ok -> ok
    end.

calc_and_send_new_mi(Mi, Y, Neighbors) ->
    NewMi = calc_new_mi(Mi, Y),
    case NewMi of
        Mi ->   ok;
        _Any -> send_new_mi(NewMi, Neighbors)
    end,
    NewMi.

calc_new_mi(Mi, Y) -> 
    case Mi > Y of
        true -> NewMi = ((Mi - 1) rem Y) + 1;
        false -> NewMi = Mi
    end,
    NewMi.

send_new_mi(NewMi, {LeftN, RightN}) ->
    LeftN ! {sendy, NewMi},
    RightN ! {sendy, NewMi}.

voteYes() ->
    ok.

tellmi(ReceiverPid, Mi) ->
    ReceiverPid ! {mi, Mi}.

pongGGT(ReceiverPid, GGTProName) ->
    ReceiverPid ! {pongGGT, GGTProName}.




hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(GGTProName, Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s.\n", [GGTProName, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).