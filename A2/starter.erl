-module(starter).

-export([
    go/1,
    go/2,

    start_all_ggtprozesse/2,

    create_ggtproname/2
]).

-define(KOPID, {koordinator,'ko@HansZimmer-PC'}).

go(StarterNummer) ->
    go(StarterNummer, ?KOPID).
go(StarterNummer, KoPid) ->
    KoPid ! {self(), getsteeringval},
    receive 
        {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProAnz} -> 
            start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz})
    end.
        
start_all_ggtprozesse(_StarterNummer, {_ArbeitsZeit, _TermZeit, _Quota, 0}) -> ok;
start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz}) ->
    GGTProName = create_ggtproname(StarterNummer, GGTProAnz),
    ggtprozess:go({GGTProName, ArbeitsZeit, TermZeit, Quota}),

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