-module(starter).

-export([
    start/1,
    start/2,
    go/1,
    go/2,

    start_all_ggtprozesse/2,

    create_ggtproname/2
]).

-define(CONFIG_DATEI_NAME, "ggt.cfg").
-define(DEFAULT_LOG_DATEI_NAME, "starter").

-define(KOORDINATORNAME, hole_wert_aus_config_mit_key(koordinatorname)).
-define(NAMESERVICENODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(NAMESERVICENAME, nameservice).

-define(PRAKTIKUMSGRUPPE, hole_wert_aus_config_mit_key(praktikumsgruppe)).
-define(TEAMNUMMER, hole_wert_aus_config_mit_key(teamnummer)).

start(ParameterFromPython) ->
    [StarterAnzahlAtom, ErsteStarterNummerAtom] = ParameterFromPython,
    
    {StarterAnzahl, []} = string:to_integer(atom_to_list(StarterAnzahlAtom)),
    {ErsteStarterNummer, []} = string:to_integer(atom_to_list(ErsteStarterNummerAtom)),
    start(StarterAnzahl, ErsteStarterNummer).

start(0, _) -> ok;
start(StarterAnzahl, ErsteStarterNummer) ->
    spawn(fun() -> go(ErsteStarterNummer) end),
    start(StarterAnzahl - 1, ErsteStarterNummer + 1).

go(StarterNummer) ->
    KoPid = get_ko_pid(StarterNummer),
    go(StarterNummer, KoPid).

go(StarterNummer, KoPid) ->
    logge_status(StarterNummer, "Startet"),
    KoPid ! {self(), getsteeringval},
    receive 
        {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProAnz} -> 
            logge_status(
                StarterNummer, 
                lists:flatten(
                    io_lib:format("steeringvals bekommen: ~p, ~p, ~p, ~p", [ArbeitsZeit, TermZeit, Quota, GGTProAnz]))),
            start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz})
    end.
        
start_all_ggtprozesse(StarterNummer, {_ArbeitsZeit, _TermZeit, _Quota, 0}) ->
    logge_status(StarterNummer, "Alle GGTProzesse gestartet");
start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz}) ->
    GGTProName = create_ggtproname(StarterNummer, GGTProAnz),
    ggtprozess:go({GGTProName, ArbeitsZeit, TermZeit, Quota}),
    logge_status(StarterNummer, lists:flatten(io_lib:format("~p wurde gestartet", [GGTProName]))),
    
    NewGGTProAnz = GGTProAnz - 1,
    start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, NewGGTProAnz}).

create_ggtproname(StarterNummer, GGTProNummer) ->
    GGTProName = io_lib:format(
                    '~p~p~p~p', 
                    [StarterNummer, ?PRAKTIKUMSGRUPPE, ?TEAMNUMMER, GGTProNummer]
                ),
    list_to_atom(lists:flatten(GGTProName)).

get_ko_pid(StarterNummer) -> 
    net_adm:ping(?NAMESERVICENODE),
    timer:sleep(timer:seconds(2)),
    case global:whereis_name(?NAMESERVICENAME) of
        undefined -> 
            logge_status(StarterNummer, "Nameservice global nicht gefunden, Starter faehrt runter"),
            timer:sleep(timer:seconds(5)),
            exit(kill);
        NsPid -> 
            NsPid ! {self(), {lookup, ?KOORDINATORNAME}},
            receive
                {pin, KoPid} -> 
                    KoPid;
                not_found -> 
                    logge_status(StarterNummer, "Koordinator nicht im Nameservice bekannt, Starter faehrt runter"),
                    timer:sleep(timer:seconds(5)),
                    exit(kill)
            end
    end.

logge_status(StarterNummer, Inhalt) ->
    LOG_DATEI_NAME = lists:flatten(io_lib:format("~s~p.log", [?DEFAULT_LOG_DATEI_NAME, StarterNummer])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("Starter~p ~p ~s.\n", [StarterNummer, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LOG_DATEI_NAME, LogNachricht).


hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_DATEI_NAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.
