-module(ggtProzessMert).

% API
-export([
        start/5
        ]).

% KONSTANTEN
-define(CONFIG_FILENAME, 	"ggt.cfg").
-define(PRAKTIKUMSGRUPPE, 	hole_wert_aus_config_mit_key(praktikumsgruppe)).
-define(TEAMNUMMER, 		hole_wert_aus_config_mit_key(teamnummer)).
-define(NSNODE,				hole_wert_aus_config_mit_key(nameservicenode)).
-define(KO, 				hole_wert_aus_config_mit_key(koordinatorname)).

%------------------------------------------------------------------------------------------------------
%										>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start(VZ, TZ, StarterNummer, ProzessNummer, Q) ->
	
	Name = ?PRAKTIKUMSGRUPPE ++ ?TEAMNUMMER ++ ProzessNummer ++ StarterNummer,
	LogDatei = erstelle_log_datei_name(Name),
	SelfPid = spawn(fun() -> init(VZ, TZ, Name, Q, LogDatei) end),
	register(Name, SelfPid),	
	logge_status(io_lib:format("Prozess mit dem Namen ~p gestartet", [Name]), LogDatei).
	
init(VZ, TZ, Name, Q, LogDatei) ->	
	KO = getPid(?KO, LogDatei),
	NS = getPid(?NSNODE, LogDatei),
	
	NS ! {self(),{rebind, node()}},
	receive
		ok ->		
			logge_status(io_lib:format("Prozess ~p wurde in den NS eingeschrieben.", [Name]), LogDatei)
	end,
    timer:sleep(timer:seconds(2)),
	
	KO ! {hello, Name},
	logge_status(io_lib:format("Prozess ~p hat sich beim KO gemeldet.", [Name]), LogDatei),
	
	receive
        {setneighbors,LeftN,RightN} ->
			Nachbarn = {lookup_name_at_ns(LeftN, NS), lookup_name_at_ns(RightN, NS)},
			logge_status(io_lib:format("Linker ~p und Rechter ~p -Nachbar wurden erhalten.", [LeftN, RightN]), LogDatei)
    end,	
	
	receive
        {setpm, MiNeu} -> 	
			Mi = MiNeu,
			logge_status(io_lib:format("Mi wurde auf ~p gesetzt.", [MiNeu]), LogDatei)
	end,
	
	prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei).

	
lookup_name_at_ns(Name, NsPid) ->
    NsPid ! {self(), {lookup, Name}},
    receive
        {pin, Pid} -> Pid;
        not_found -> not_found
    end.
	
getPid(Node, LogDatei) -> 
    net_adm:ping(Node),
    timer:sleep(timer:seconds(2)),
    case global:whereis_name(Node) of
        undefined ->            
			logge_status(io_lib:format("~p nicht gefunden, ggt wird beendet.", [Node]), LogDatei),
            timer:sleep(timer:seconds(5)),
            exit(kill);
        Pid -> Pid
    end.
%------------------------------------------------------------------------------------------------------
%								>>LOOPS<<
%------------------------------------------------------------------------------------------------------
prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei) -> 
	prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, 0, {false, false}).

prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, VoteCount, VoteCfg) -> 
	{TermFlag, VoteFlag} = VoteCfg,
	receive
        {setpm, MiNeu} -> 				
			logge_status(io_lib:format("Neues Mi: ~p erhalten.", [MiNeu]), LogDatei),
			prozess_loop(VZ, TZ, NS, KO, Name, MiNeu, Nachbarn, Q, LogDatei, 0, {false, false});
		 
		{sendy, Y} ->
			MiNeu = calcGGT(Mi, Y, Nachbarn, KO, Name, LogDatei),
			prozess_loop(VZ, TZ, NS, KO, Name, MiNeu, Nachbarn, Q, LogDatei, VoteCount, {TermFlag, false});
			
		{From, tellmi} ->
			From ! {mi, Mi},			
			logge_status(io_lib:format("Mi an ~p gesendet.", [From]), LogDatei),
			prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, VoteCount, VoteCfg);
			
		{From, pingGGT} -> 
			From ! {pongGGT, Name},			
			logge_status(io_lib:format("Ping von ~p erhalten.", [From]), LogDatei),
			prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, VoteCount, VoteCfg);
		
		{voteYes, Name} ->
			{NewTerm, NewVoteCount, BriefFlag} = handleIncVote(TermFlag, Q, VoteCount),			
			logge_status(io_lib:format("Vote von ~p erhalten.", [Name]), LogDatei),
			case BriefFlag of
				true ->
					briefTerm(KO, Name, Mi, LogDatei)					
			end,			
			prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, NewVoteCount, {NewTerm, VoteFlag});
		
		{From, {vote, Initiator}} ->   
			case TermFlag of %TODO TZ / 2
				true -> 
					From ! {voteYes, Name},
					logge_status(io_lib:format("Vote Anfrage von ~p erhalten und mit Ja gestimmt.", [Initiator]), LogDatei)
			end,			
			prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, VoteCount, VoteCfg);
			
		kill -> 
			kill(Name, NS, LogDatei)
     
        after timer:seconds(TZ) ->  
			case VoteFlag of
				false ->
					case TermFlag of
						false ->
							NS ! {self(), {multicast, vote, Name}},					
							prozess_loop(VZ, TZ, NS, KO, Name, Mi, Nachbarn, Q, LogDatei, 0, {TermFlag, true})
					end
			end		
			
    end.

%------------------------------------------------------------------------------------------------------
%								>>EIGENTLICHE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------

calcGGT(Mi, Y, {LeftN, RightN}, KO, Name, LogDatei) -> 
    case Mi > Y of
        true -> 
			NewMi = ((Mi - 1) rem Y) + 1,			
			logge_status(io_lib:format("Neues Mi: ~p wurde Berechnet der KOORDINATOR und die Nachbarn wurde bescheid gesagt.", [NewMi]), LogDatei),
			LeftN !  {sendy, NewMi},
			RightN ! {sendy, NewMi},
			KO !  {briefmi, {Name, NewMi, vsutil:now2string(erlang:timestamp())}};
        false -> 
			NewMi = Mi
    end,
    NewMi.
	
handleIncVote(Term, Q, VoteCount) ->
	NewVoteCount = VoteCount + 1,	
	case Term of
		false ->
			case (NewVoteCount >= Q) of
				true ->
					Ret ={true, NewVoteCount, true};
				false ->
					Ret = {false, NewVoteCount, false}
			end;
		true ->
			Ret = {true, NewVoteCount, false}
	end,
	Ret.
					
briefTerm(KO, Name, Mi, LogDatei) ->
    KO ! {self(), briefterm, {Name, Mi, vsutil:now2string(erlang:timestamp())}},
	logge_status("Genug Votes erhalten und Terminiert.", LogDatei).
	
kill(Name, NsPid, LogDatei) ->
    NsPid ! {self(), {unbind, Name}},
    receive
        ok -> 
			logge_status("Aus dem NS entfernt.", LogDatei)
    end,
	unregister(Name),    
	logge_status(io_lib:format("~p ist heruntergefahren.", [Name]), LogDatei),
	exit(kill).
	

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

erstelle_log_datei_name(Name) ->
    LogDatei = "client" ++ io_lib:format("~p",[Name]) ++ ".log",
    LogDatei.

% Loggt uebergebene Nachrichten
logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = erlang:timestamp(),
    LogNachricht = io_lib:format("~p ~p.\n", [vsutil:now2string(AktuelleZeit), Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).