-module(koordinator).

-export([
    start/0,
    start/1,

    wait_for_starters/2,
    wait_and_collect_ggtpro/2,
    create_circle/2,
    set_neighbors/4,
    get_next_to_last_and_last_elem/1,

    calculation_receive_loop/2,
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
    toggle/0,
    kill/2,
    kill_all_ggtprocesses/2,

    ggtpropid_exists/2,
    get_ggtpropid/2
]).

-define(CONFIG_DATEI_NAME, "koordinator.cfg").
-define(LOG_DATEI_NAME, "koordinator.log").
-define(NSPID, hole_wert_aus_config_mit_key(nspid)).
-define(KONAME, hole_wert_aus_config_mit_key(koname)).
-define(STARTER_COUNT, hole_wert_aus_config_mit_key(startercount)).
-define(ARBEITSZEIT, hole_wert_aus_config_mit_key(arbeitszeit)).
-define(TERMZEIT, hole_wert_aus_config_mit_key(termzeit)).
-define(QUOTA, hole_wert_aus_config_mit_key(quota)).
-define(GGTPROANZ, hole_wert_aus_config_mit_key(ggtproanz)).
-define(STARTER_STEERINGVAL_TIMEOUT, hole_wert_aus_config_mit_key(starter_steeringval_timeout)).


start() ->
    start(?NSPID).

start(NsPid) ->
    logge_status("koordinator startet"), 

    SollGGTCount = ?GGTPROANZ * ?STARTER_COUNT,
    case SollGGTCount >= 3 of
        true -> continue;
        false -> throw_error("GGTPROANZ ist ~p, sollte aber mindestens 3 sein (für Kreis wichtig)", [?GGTPROANZ])
    end,
    SollQuota = round((SollGGTCount * ?QUOTA) / 100),

    register(?KONAME, self()),
    NsPid ! {self(), {bind, ?KONAME, node()}},
    receive
        ok -> logge_status("ist registriert und beim nameservice bekannt")
    end,
    start_starters(?STARTER_COUNT),
    StartersCount = wait_for_starters({steeringval, ?ARBEITSZEIT, ?TERMZEIT, SollQuota, ?GGTPROANZ}, 0),
    logge_status(lists:flatten(io_lib:format("~p von ~p starter(s) haben steeringval angefragt", [StartersCount, ?STARTER_COUNT]))), 

    GlobalGGTProAnz = StartersCount * ?GGTPROANZ,
    logge_status(lists:flatten(io_lib:format("Es laufen ~p von ~p GGTProzessen", [GlobalGGTProAnz, SollGGTCount]))), 

    GGTProNameList = wait_and_collect_ggtpro([], GlobalGGTProAnz),
    logge_status(lists:flatten(io_lib:format("~p ggT-Prozesse sind bekannt", [length(GGTProNameList)]))), 

    receive
        step -> create_circle(GGTProNameList, NsPid)
    end,
    logge_status("kreis erstellt"),
    receive
        {calc, WggT} -> logge_status("got calc"),
                        calc(WggT, GGTProNameList, NsPid),
                        logge_status("calc init done"),
                        calculation_receive_loop(GGTProNameList, NsPid)
    end.

start_starters(0) -> ok;
start_starters(RestCount) ->
    spawn(fun() -> starter:go(RestCount) end),
    start_starters(RestCount - 1).

wait_for_starters(SteeringValues, CurrentStartersCount) ->
    receive
        {AbsenderPid, getsteeringval} ->
            AbsenderPid ! SteeringValues,
            NextStartersCount = CurrentStartersCount + 1,
            wait_for_starters(SteeringValues, NextStartersCount)
        after ?STARTER_STEERINGVAL_TIMEOUT -> CurrentStartersCount
    end.

wait_and_collect_ggtpro(GGTProNameList, 0) -> 
    GGTProNameList;
wait_and_collect_ggtpro(GGTProNameList, RestGGTProCount) ->
    receive
        {hello, GGTProName} -> 
            NewGGTProNameList = [GGTProName | GGTProNameList],
            NewRestGGTProCount = RestGGTProCount - 1,
            wait_and_collect_ggtpro(NewGGTProNameList, NewRestGGTProCount)
    end.


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

create_circle(GGTProNameList, NsPid) ->
    [FirstGGTProName, SecondGGTProName | _RestGGTProNames] = GGTProNameList,
    [NextToLastGGTProName, LastGGTProName] = get_next_to_last_and_last_elem(GGTProNameList),
    set_neighbors(FirstGGTProName, LastGGTProName, SecondGGTProName, NsPid),
    set_neighbors(LastGGTProName, NextToLastGGTProName, FirstGGTProName, NsPid),
    create_circle_(GGTProNameList, NsPid).

create_circle_([_NextToLastGGTProName, _LastGGTProName], _NsPid) -> ok;
create_circle_([FirstGGTProName, SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid) ->
    set_neighbors(SecondGGTProName, FirstGGTProName, ThirdGGTProName, NsPid),
    create_circle_([SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid).

set_neighbors(MiddleGGTProName, LeftGGTProName, RightGGTProName, NsPid) ->
    case ggtpropid_exists(MiddleGGTProName, NsPid) of
        true ->     continue;
        false ->  
            throw(ggtpronameUnkownForNs)  
            %log_status("Circle kann nicht vervollständigt werden, ~p wurde beim nameservice nicht gefunden!", [MiddleGGTProName])
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

calculation_receive_loop(GGTProNameList, NsPid) ->
    receive
        {briefmi, {GGTProName, CMi, CZeit}} -> 
            logge_status("got briefmi"),
            briefmi(GGTProName, CMi, CZeit),
            calculation_receive_loop(GGTProNameList, NsPid);
        {AbsenderPid, briefterm, {GGTProName, CMi, CZeit}} -> 
            logge_status("got briefterm"),
            briefterm(AbsenderPid, GGTProName, CMi, CZeit),
            calculation_receive_loop(GGTProNameList, NsPid); 
        prompt ->   
            logge_status("got prompt"),
            prompt(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid);
        nudge ->    
            logge_status("got nudge"),
            nudge(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid);
        toggle ->   
            logge_status("got toggle"),
            toggle(),
            calculation_receive_loop(GGTProNameList, NsPid);
        reset ->    
            logge_status("got reset"),
            reset(GGTProNameList, NsPid);
        kill ->     
            logge_status("got kill"),
            kill(GGTProNameList, NsPid)
    end.


briefmi(GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, false).

briefterm(_AbsenderPid, GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, true).
    %{sendy, LCMi}, wirklich etwas zurück senden?

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
            %throw_error("GGTProName ~p beim nameservice unbekannt!", [HeadGGTProName])
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

toggle() ->
    throw("Not yet implemented").

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
    case whereis(?KONAME) of
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
        {pin, GGTProPid} -> GGTProPid;
        not_found -> 
            throw(ggtpronameUnkownForNs)
            %throw_error("GGTProName ~p beim nameservice unbekannt! Nutze vor dieser Funktion ggtpropid_exists/2 um sicher zugehen!", [GGTProName])
    end.

throw_error(Text, List) ->
    throw_error(lists:flatten(io_lib:format(Text,List))).
throw_error(Text) ->
    throw(Text).

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