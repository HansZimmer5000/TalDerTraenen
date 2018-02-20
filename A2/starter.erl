-module(starter).

-export([
    go/1,

    start_all_ggtprozesse/2,

    create_ggtproname/2
]).

-define(KOPID, {koordinator,'ko@HansZimmer-PC'}).

go(StarterNummer) ->
    KOPid = ?KOPID,
    KOPid ! {self(), getsteeringval},
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
                    "GGT-~p~p~p~p", 
                    [StarterNummer, Praktikumsgruppennummer, Teamnummer, GGTProNummer]
                ),
    GGTProName.