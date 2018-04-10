-module(starter).

-export([
    start/1,
    go/1,
    go/2,

    start_all_ggtprozesse/2,

    create_ggtproname/2
]).

-define(CONFIG_DATEI_NAME, "ggt.cfg").
-define(DEFAULT_LOG_DATEI_NAME, "starter").

-define(KOORDINATORNAME, hole_wert_aus_config_mit_key(koordinatorname)).
-define(KOPID, whereis(?KOORDINATORNAME)).

-define(PRAKTIKUMSGRUPPE, hole_wert_aus_config_mit_key(praktikumsgruppe)).
-define(TEAMNUMMER, hole_wert_aus_config_mit_key(teamnummer)).

start([ParameterFromPython]) ->
    {StarterNummer, []} = string:to_integer(atom_to_list(ParameterFromPython)),
    go(StarterNummer);
start(StarterNummer) ->
    go(StarterNummer).

go(StarterNummer) ->
    go(StarterNummer, ?KOPID).
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
                    'ggt-~p~p~p~p', 
                    [StarterNummer, ?PRAKTIKUMSGRUPPE, ?TEAMNUMMER, GGTProNummer]
                ),
    list_to_atom(lists:flatten(GGTProName)).


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
