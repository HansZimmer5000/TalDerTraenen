-module(starter).

-export([
    start/1,
    go/1,
    go/2,

    start_all_ggtprozesse/2,

    create_ggtproname/2
]).

-define(KOPID, {koordinator,'ko@HansZimmer-PC'}).
-define(DEFAULT_LOG_DATEI_NAME, "starter").

start(StarterNummer) ->
    go(StarterNummer, ?KOPID).

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
    logge_status(StarterNummer, lists:flatten(io_lib:format("GGTPro~p gestartet", [GGTProAnz]))),
    
    NewGGTProAnz = GGTProAnz - 1,
    start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, NewGGTProAnz}).

create_ggtproname(StarterNummer, GGTProNummer) ->
    Praktikumsgruppennummer = 1,
    Teamnummer = 2,
    GGTProName = io_lib:format(
                    'ggt-~p~p~p~p', 
                    [StarterNummer, Praktikumsgruppennummer, Teamnummer, GGTProNummer]
                ),
    list_to_atom(lists:flatten(GGTProName)).


logge_status(StarterNummer, Inhalt) ->
    LOG_DATEI_NAME = lists:flatten(io_lib:format("~s~p.log", [?DEFAULT_LOG_DATEI_NAME, StarterNummer])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("Starter~p ~p ~s.\n", [StarterNummer, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LOG_DATEI_NAME, LogNachricht).