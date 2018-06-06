-module(starterMert).

-export([
    start/1
]).

-define(CONFIG_FILENAME, "c:/Users/Mert.S/eclipse-workspace/VSP2/src/ggt.cfg").
-define(DEFAULT_LOG_DATEI_NAME, "starter").
-define(KOORDINATORNAME, hole_wert_aus_config_mit_key(koordinatorname)).
-define(NAMESERVICENODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(NAMESERVICENAME, nameservice).
-define(PRAKTIKUMSGRUPPE, hole_wert_aus_config_mit_key(praktikumsgruppe)).
-define(TEAMNUMMER, hole_wert_aus_config_mit_key(teamnummer)).


%------------------------------------------------------------------------------------------------------
%										>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start(StarterNummer) ->
    logge_status(StarterNummer, "Startet"),	
	KoPid = get_ko_pid(StarterNummer),
    KoPid ! {self(), getsteeringval},
    receive 
        {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProAnz} ->  
			logge_status(StarterNummer, lists:flatten(io_lib:format("steeringvals bekommen: ~p, ~p, ~p, ~p", [ArbeitsZeit, TermZeit, Quota, GGTProAnz]))),
            start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz})
    end.
	
%------------------------------------------------------------------------------------------------------
%										>>LOOPS<<
%------------------------------------------------------------------------------------------------------
start_all_ggtprozesse(StarterNummer, {_ArbeitsZeit, _TermZeit, _Quota, 0}) ->
    logge_status(StarterNummer, "Alle GGTProzesse gestartet");
	
start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, GGTProAnz}) ->
    GGTProName = create_ggtproname(StarterNummer, GGTProAnz),	
    ggtprozess:start(ArbeitsZeit, TermZeit, StarterNummer, GGTProAnz, Quota) ,
    logge_status(StarterNummer, lists:flatten(io_lib:format("~p wurde gestartet", [GGTProName]))),    
    NewGGTProAnz = GGTProAnz - 1,
    start_all_ggtprozesse(StarterNummer, {ArbeitsZeit, TermZeit, Quota, NewGGTProAnz}).


%------------------------------------------------------------------------------------------------------
%								>>FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------	
create_ggtproname(StarterNummer, GGTProNummer) ->
    GGTProName = io_lib:format('ggt-~p~p~p~p', [StarterNummer, ?PRAKTIKUMSGRUPPE, ?TEAMNUMMER, GGTProNummer]),
    GGTProName.
	
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

%------------------------------------------------------------------------------------------------------
%										>>GENERELLE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

%------------------------------------------------------------------------------------------------------	
%										>>LOGGING FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------	
logge_status(StarterNummer, Inhalt) ->
    LOG_DATEI_NAME = lists:flatten(io_lib:format("~s~p.log", [?DEFAULT_LOG_DATEI_NAME, StarterNummer])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("Starter~p ~p ~s.\n", [StarterNummer, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LOG_DATEI_NAME, LogNachricht).	
	