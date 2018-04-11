-module(koordinator).

-export([
    start/0,
    start/1,

    init_loop/4,
    calc_quote/2,
    create_circle/2,
    set_neighbors/4,
    get_next_to_last_and_last_elem/1,

    calculation_receive_loop/3,
    briefmi/3,
    briefterm/4,
    reset/2,
    calc/3,
    get_pms/2,
    select_random_some_ggtprocesses/1,
    send_pms_to_ggtprocesses/3,
    send_ys_to_ggtprocesses/3,
    get_first_n_elems_of_list/3,
    send_message_to_processname/3,
    prompt/2,
    nudge/2,
    toggle/1,
    kill/2,
    kill_all_ggtprocesses/2,

    ggtpropid_exists/2,
    get_ggtpropid/2
]).

-define(CONFIG_DATEI_NAME, "koordinator.cfg").
-define(LOG_DATEI_NAME, "koordinator.log").

-define(NSPID, global:whereis_name(nameservice)).
-define(NSNODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(KONAME, hole_wert_aus_config_mit_key(koordinatorname)).

-define(STARTER_COUNT, hole_wert_aus_config_mit_key(startercount)).
-define(ARBEITSZEIT, hole_wert_aus_config_mit_key(arbeitszeit)).
-define(TERMZEIT, hole_wert_aus_config_mit_key(termzeit)).
-define(QUOTA, hole_wert_aus_config_mit_key(quota)).
-define(GGTPROANZ, hole_wert_aus_config_mit_key(ggtprozessnummer)).
-define(KORRIGIEREN, hole_wert_aus_config_mit_key(korrigieren)).


start() ->
    net_adm:ping(?NSNODE), 
    timer:sleep(timer:seconds(2)),
    start(?NSPID).

start(NsPid) ->
    logge_status("koordinator startet"), 

    net_adm:ping(?NSNODE),    

    register_at_ns(NsPid),

    start_starters(?STARTER_COUNT),

    SteeringValues = {steeringval, ?ARBEITSZEIT, ?TERMZEIT, 0, ?GGTPROANZ},

    GGTProNameList = init_loop(NsPid, SteeringValues, 0, []),

    receive
        {calc, WggT} -> calc(WggT, GGTProNameList, NsPid),
                        logge_status("calc init done"),
                        calculation_receive_loop(GGTProNameList, NsPid, ?KORRIGIEREN)
    end.

register_at_ns(undefined) ->
    logge_status("NsPid unbekannt");
register_at_ns(NsPid) ->
    register(?KONAME, self()),
    NsPid ! {self(), {rebind, ?KONAME, node()}},
    receive
        ok -> logge_status("ist registriert und beim nameservice bekannt")
    end.

start_starters(0) -> ok;
start_starters(RestCount) ->
    spawn(fun() -> starter:go(RestCount) end),
    start_starters(RestCount - 1).

init_loop(NsPid, SteeringValues, CurrentStartersCount, GGTProNameList) ->
    receive
        {AbsenderPid, getsteeringval} ->
            NewStartersCount = CurrentStartersCount + 1,
            {steeringval, Arbeitszeit, Termzeit, _OldQuote, GGTProAnz} = SteeringValues,
            NewQuote = calc_quote(NewStartersCount, GGTProAnz),
            NewSteeringValues = {steeringval, Arbeitszeit, Termzeit, NewQuote, GGTProAnz},

            AbsenderPid ! NewSteeringValues,
            init_loop(NsPid, NewSteeringValues, NewStartersCount, GGTProNameList);
        {hello, GGTProName} ->
            NewGGTProNameList = [GGTProName | GGTProNameList],
            init_loop(NsPid, SteeringValues, CurrentStartersCount, NewGGTProNameList);
        step ->
            SollGGTCount = ?STARTER_COUNT * ?GGTPROANZ,
            logge_status(lists:flatten(io_lib:format("~p von ~p starter(s) haben steeringval angefragt", [CurrentStartersCount, ?STARTER_COUNT]))), 
            logge_status(lists:flatten(io_lib:format("Es laufen ~p von ~p GGTProzessen", [length(GGTProNameList), SollGGTCount]))),
            
            create_circle(GGTProNameList, NsPid),
            GGTProNameList
    end.

calc_quote(StartersCount, GGTProAnz) -> 
    round(StartersCount * GGTProAnz * 80 / 100).

create_circle(GGTProNameList, NsPid) ->
    [FirstGGTProName, SecondGGTProName | _RestGGTProNames] = GGTProNameList,
    [NextToLastGGTProName, LastGGTProName] = get_next_to_last_and_last_elem(GGTProNameList),
    set_neighbors(FirstGGTProName, LastGGTProName, SecondGGTProName, NsPid),
    set_neighbors(LastGGTProName, NextToLastGGTProName, FirstGGTProName, NsPid),
    create_circle_(GGTProNameList, NsPid).

create_circle_([_NextToLastGGTProName, _LastGGTProName], _NsPid) -> 
    logge_status("kreis erstellt");
create_circle_([FirstGGTProName, SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid) ->
    set_neighbors(SecondGGTProName, FirstGGTProName, ThirdGGTProName, NsPid),
    create_circle_([SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid).

calc(WggT, GGTProNameList, NsPid) ->
    PMList = get_pms(WggT, GGTProNameList),
    send_pms_to_ggtprocesses(PMList, GGTProNameList, NsPid),
    logge_status("pms zu ggT-Prozessen gesendet"),
    SelectedGGTProcesses = select_random_some_ggtprocesses(GGTProNameList),
    send_ys_to_ggtprocesses(PMList, SelectedGGTProcesses, NsPid),
    logge_status(lists:flatten(io_lib:format("ys zu ~p ggT-Prozessen gesendet", [length(SelectedGGTProcesses)]))).

get_pms(WggT, GGTProNameList) ->
    GGTProAnz = length(GGTProNameList),
    PMList = vsutil:bestimme_mis(WggT, GGTProAnz),
    PMList.

select_random_some_ggtprocesses(GGTProNameList) ->
    ShuffledGGTProNameList = util:shuffle(GGTProNameList),
    SelectionCount = round(length(GGTProNameList) / 5),
    case SelectionCount < 2 of
        true ->  
            Result = get_first_n_elems_of_list(2, ShuffledGGTProNameList, []);
        false -> 
            Result = get_first_n_elems_of_list(SelectionCount, ShuffledGGTProNameList, [])
    end,
    Result.

send_pms_to_ggtprocesses([], [], _NsPid) -> ok;
send_pms_to_ggtprocesses([HeadPM | RestPMs], [HeadGGTProName | RestGGTProNames], NsPid) ->
    send_message_to_processname({setpm, HeadPM}, HeadGGTProName, NsPid),
    send_pms_to_ggtprocesses(RestPMs, RestGGTProNames, NsPid).

send_ys_to_ggtprocesses(_Ys, [], _NsPid) -> done;
send_ys_to_ggtprocesses([HeadY | RestYs], [HeadGGTProName | RestGGTProNames], NsPid) ->
    send_message_to_processname({sendy, HeadY}, HeadGGTProName, NsPid),
    send_ys_to_ggtprocesses(RestYs, RestGGTProNames, NsPid).

% The order of the first n elements is switched in the Result! [1,2] -> [2,1]
get_first_n_elems_of_list(0, _List, Akku) -> Akku;
get_first_n_elems_of_list(N, [Head | Rest], Akku) ->
    NewAkku = [Head | Akku],
    NewN = N - 1,
    get_first_n_elems_of_list(NewN, Rest, NewAkku).

send_message_to_processname(Message, ProName, NsPid) ->
    case ggtpropid_exists(ProName, NsPid) of
        true -> continue;
        false -> throw(ggtpronameUnkownForNs)
    end,
    ProPid = get_ggtpropid(ProName, NsPid),
    ProPid ! Message.


set_neighbors(MiddleGGTProName, LeftGGTProName, RightGGTProName, NsPid) ->
    case ggtpropid_exists(MiddleGGTProName, NsPid) of
        true ->     
            continue;
        false ->  
            throw(ggtpronameUnkownForNs)  
    end,
    MiddleGGTProPid = get_ggtpropid(MiddleGGTProName, NsPid),
    MiddleGGTProPid ! {setneighbors, LeftGGTProName, RightGGTProName}.

get_next_to_last_and_last_elem([]) ->
    [];
get_next_to_last_and_last_elem([_OneElem]) ->
    get_next_to_last_and_last_elem([]);
get_next_to_last_and_last_elem([NextToLastElem, LastElem]) -> 
    [NextToLastElem, LastElem];
get_next_to_last_and_last_elem([_HeadElem | RestElems]) ->
    get_next_to_last_and_last_elem(RestElems).

calculation_receive_loop(GGTProNameList, NsPid, Korrigieren) ->
    receive
        {briefmi, {GGTProName, CMi, CZeit}} -> 
            briefmi(GGTProName, CMi, CZeit),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren);
        {AbsenderPid, briefterm, {GGTProName, CMi, CZeit}} -> 
            briefterm(AbsenderPid, GGTProName, CMi, CZeit),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren); 
        prompt ->   
            prompt(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren);
        nudge ->    
            nudge(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren);
        toggle ->   
            NeuesKorrigieren = toggle(Korrigieren),
            calculation_receive_loop(GGTProNameList, NsPid, NeuesKorrigieren);
        reset ->    
            reset(GGTProNameList, NsPid);
        kill ->     
            kill(GGTProNameList, NsPid)
    end.


briefmi(GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, false).

briefterm(_AbsenderPid, GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, true).
    %{sendy, LCMi}, TODO: Rücksenden (globales minimales ggT) wenn Korrigieren = true

prompt(GGTProNameList, NsPid) ->
    send_and_receive_mi(GGTProNameList, NsPid).

send_and_receive_mi([], _NsPid) -> done;
send_and_receive_mi([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> continue;
        false -> 
            throw(ggtpronameUnkownForNs)
            %throw_error("GGTProName ~p beim nameservice unbekannt!", [HeadGGTProName])
    end,
    HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
    HeadGGTProPid ! {self(), tellmi},
    receive
        {mi, Mi} -> logge_ggtpro_status(HeadGGTProName, Mi) 
    end,
    send_and_receive_mi(RestGGTProNames, NsPid).


nudge([], _NsPid) -> ok;
nudge([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> continue;
        false -> 
            throw(ggtpronameUnkownForNs)
    end,
    HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
    HeadGGTProPid ! {self(),pingGGT},
    receive 
        {pongGGT, HeadGGTProName} -> 
            logge_status(lists:flatten(io_lib:format("~p pong bekommen", [HeadGGTProName])))
        after 2000 -> 
            logge_status(lists:flatten(io_lib:format("GGTProName ~p mit ~p meldet sich nicht!", [HeadGGTProName, HeadGGTProPid])))
    end,
    nudge(RestGGTProNames, NsPid).

toggle(Korrigieren) ->
    not(Korrigieren).

reset(GGTProNameList, NsPid) ->
    finalize(GGTProNameList, NsPid, true).

kill(GGTProNameList, NsPid) ->
    finalize(GGTProNameList, NsPid, false).

finalize(GGTProNameList, NsPid, Restart) ->
    kill_all_ggtprocesses(GGTProNameList, NsPid),
    NsPid ! {self(), {unbind, ?KONAME}},
    receive
        ok -> continue
    end,
    case global:whereis_name(?KONAME) of
        undefined -> logge_status("finalize findet ?KONAME nicht, wenn Test -> ok"),
                     ok; %Only for Test purposes! Because since its in the same process the name is always registered during normal run until unregistered here.
        _Any -> unregister(?KONAME)
    end,
    logge_status(lists:flatten(io_lib:format("Alle GGT-Prozesse herunter gefahren, selbst unregistered, Neustart = ~p", [Restart]))),
    case Restart of
        true -> start(NsPid);
        false -> finish
    end.


kill_all_ggtprocesses([], _NsPid) -> done;
kill_all_ggtprocesses([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> 
            HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
            HeadGGTProPid ! kill,
            kill_all_ggtprocesses(RestGGTProNames, NsPid);
        false -> 
            kill_all_ggtprocesses(RestGGTProNames, NsPid)
    end.

%------------
ggtpropid_exists(GGTProName, NsPid) ->
    NsPid ! {self(), {lookup, GGTProName}},
    receive
        {pin, _GGTProPid} -> true;
        not_found -> false
    end.
get_ggtpropid(GGTProName, NsPid) ->
    NsPid ! {self(), {lookup, GGTProName}},
    receive
        {pin, GGTProPid} -> 
            GGTProPid;
        not_found -> 
            throw(ggtpronameUnkownForNs)
    end.

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_DATEI_NAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_ggtpro_status(GGTProName, CMi) ->
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi)", 
                            [GGTProName, CMi])
                    ),
    logge_status(LogNachricht),
    LogNachricht.

logge_ggtpro_status(GGTProName, CMi, CZeit, TermFlag) ->
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi) ~p(TermFlag) ~p(CZeit)", 
                            [GGTProName, CMi, TermFlag, CZeit])
                    ),
    logge_status(LogNachricht),
    LogNachricht.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).