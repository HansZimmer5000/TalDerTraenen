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
    prompt/2,
    nudge/2,
    toggle/0,
    kill/2,
    kill_all_ggtprocesses/2,

    ggtpropid_exists/2,
    get_ggtpropid/2
]).

-define(NSPID, {nameservice,'ns@HansZimmer-PC'}).
-define(KONAME, koordinator).
-define(LOG_DATEI_NAME, "koordinator.log").
-define(ARBEITSZEIT, 0).
-define(TERMZEIT, 0).
-define(QUOTA, 2).
-define(GGTPROANZ, 5).

% WARNING! Ausgelegt nur für einen Starter!! (ein Recieveblock in start/0)
%1. start()
%2. register
%3. bind
%4. warte auf steeringval anfrage
%5. warte auf alle ggts (hallo)
%6. mache Kreis
%7. receive

start() ->
    start(?NSPID).

start(NsPid) ->

    case ?GGTPROANZ >= 3 of
        true -> continue;
        false -> 
            io:fwrite("GGTPROANZ ist ~p, sollte aber mindestens 3 sein (für Kreis wichtig)", [?GGTPROANZ]),
            true = false
    end,

    register(?KONAME, self()),
    NsPid ! {self(), {bind, ?KONAME, node()}},
    receive
        ok -> continue
    end,
    StartersCount = wait_for_starters({steeringval, ?ARBEITSZEIT, ?TERMZEIT, ?QUOTA, ?GGTPROANZ}, 0),
    GlobalGGTProAnz = StartersCount * ?GGTPROANZ,
    GGTProNameList = wait_and_collect_ggtpro([], GlobalGGTProAnz),
    receive
        step -> create_circle(GGTProNameList, NsPid)
    end,
    receive
        {calc, WggT} -> calc(WggT, GGTProNameList, NsPid),
                        calculation_receive_loop(GGTProNameList, NsPid)
    end.

wait_for_starters(SteeringValues, CurrentStartersCount) ->
    receive
        {AbsenderPid, getsteeringval} ->
            AbsenderPid ! SteeringValues,
            NextStartersCount = CurrentStartersCount + 1,
            wait_for_starters(SteeringValues, NextStartersCount)
        after 3 -> CurrentStartersCount
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


calc(_WggT, _GGTProNameList, _NsPid) ->
    io:fwrite("Not yet implemented"),
    true = false.

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
            io:fwrite("Circle kann nicht vervollständigt werden, ~p wurde beim nameservice nicht gefunden!", [MiddleGGTProName]),
            true = false
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
        {briefmi, {GGTProName, CMi, CZeit}} -> briefmi(GGTProName, CMi, CZeit);
        {AbsenderPid, briefterm, {GGTProName, CMi, CZeit}} -> briefterm(AbsenderPid, GGTProName, CMi, CZeit); 
        reset -> reset(GGTProNameList, NsPid);
        prompt -> prompt(GGTProNameList, NsPid);
        nudge -> nudge(GGTProNameList, NsPid);
        toggle -> toggle();
        kill -> kill(GGTProNameList, NsPid)
    end.


briefmi(GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, false).

briefterm(_AbsenderPid, GGTProName, CMi, CZeit) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, true).
    %{sendy, LCMi}, wirklich etwas zurück senden?

reset(GGTProNameList, NsPid) ->
    kill_all_ggtprocesses(GGTProNameList, NsPid),
    NsPid ! {self(), {unbind, ?KONAME}},
    receive
        ok -> continue
    end,
    unregister(?KONAME),
    start(NsPid).

prompt(GGTProNameList, NsPid) ->
    send_and_receive_mi(GGTProNameList, NsPid).

send_and_receive_mi([], _NsPid) -> done;
send_and_receive_mi([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> continue;
        false -> io:fwrite("GGTProName ~p beim nameservice unbekannt!", [HeadGGTProName]),
                 true = false
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
            io:fwrite("GGTProName ~p beim nameservice unbekannt!", [HeadGGTProName]),
            true = false
    end,
    HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
    HeadGGTProPid ! {self(),pingGGT},
    receive 
        {pongGGT, HeadGGTProName} -> ok
        after 2 -> io:fwrite("GGTProName ~p mit ~p meldet sich nicht!", [HeadGGTProName, HeadGGTProPid])
    end,
    nudge(RestGGTProNames, NsPid).

toggle() ->
    io:fwrite("Not yet implemented"),
    true = false.

kill(GGTProNameList, NsPid) ->
    kill_all_ggtprocesses(GGTProNameList, NsPid),
    NsPid ! {self(), {unbind, ?KONAME}},
    receive
        ok -> finish
    end.

kill_all_ggtprocesses([], _NsPid) -> done;
kill_all_ggtprocesses([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> 
            HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
            exit(HeadGGTProPid, kill),
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
            io:fwrite("GGTProName ~p beim nameservice unbekannt! Nutze vor dieser Funktion ggtpropid_exists/2 um sicher zugehen!", [GGTProName]),
            true = false
    end.


logge_ggtpro_status(GGTProName, CMi) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi) um ~p.\n", 
                            [GGTProName, CMi, AktuelleZeit])
                    ),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht),
    LogNachricht.
logge_ggtpro_status(GGTProName, CMi, CZeit, TermFlag) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi) ~p(TermFlag) ~p(CZeit) um ~p.\n", 
                            [GGTProName, CMi, TermFlag, CZeit, AktuelleZeit])
                    ),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht),
    LogNachricht.