-module(ggtprozess).

-export([
    go/1,
    init/2,

    receive_loop/2,

    vote/6,
    kill/2,
    calc_and_send_new_mi/5,
    send_new_mi/4,
    vote_yes/3,
    start_vote/3,
    tellmi/2,
    pongGGT/2,
    calc_new_mi/2
]).

-define(CONFIG_FILENAME, "ggt.cfg").

-define(NSNODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(NSNAME, nameservice).

-define(KOORDINATORNAME, hole_wert_aus_config_mit_key(koordinatorname)).


go({GGTProName, ArbeitsZeit, TermZeit, Quota}) ->
    {KoPid, NsPid} = get_ko_and_ns_pid(GGTProName),
    go({GGTProName, ArbeitsZeit, TermZeit, Quota, NsPid, KoPid});

go({GGTProName, ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    GlobalVariables = {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid},

    GGTProPid = spawn(fun() -> init(GGTProName, GlobalVariables) end),
    true = register(GGTProName, GGTProPid),
    logge_status(GGTProName, lists:flatten(
                                io_lib:format("gestartet mit PID ~p",[GGTProPid]))),
    GGTProPid.

get_ko_and_ns_pid(GGTProName) -> 
    net_adm:ping(?NSNODE),
    timer:sleep(timer:seconds(2)),
    case global:whereis_name(?NSNAME) of
        undefined -> 
            logge_status(GGTProName, "Nameservice global nicht gefunden, ggT faehrt runter"),
            timer:sleep(timer:seconds(5)),
            exit(kill);
        NsPid -> 
            KoPid = lookup_name_at_ns(?KOORDINATORNAME, NsPid),
            case KoPid of
                not_found -> 
                    logge_status(GGTProName, "Koordinator nicht im Nameservice bekannt, ggT faehrt runter"),
                    timer:sleep(timer:seconds(5)),
                    exit(kill);
                _ -> {KoPid, NsPid}
            end
    end.

init(GGTProName, {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}) ->
    NsPid ! {self(), {rebind, GGTProName, node()}},
    receive
        ok -> logge_status(GGTProName, "registriert und bekannt beim nameservice")
    end,
    KoPid ! {hello, GGTProName},

    receive
        {setneighbors, LeftN, RightN} ->  
            logge_status(GGTProName, io_lib:format("Nachbarn bekommen: ~p (Left) ~p (Right)",[LeftN, RightN])),

            LeftPid = lookup_name_at_ns(LeftN, NsPid),
            RightPid = lookup_name_at_ns(RightN, NsPid),

            case LeftPid of 
                not_found -> logge_status(GGTProName, "LeftN Pid not_found in nameservice, Fahre runter"), exit(kill);
                _ -> continue
            end,
            case RightPid of 
                not_found -> logge_status(GGTProName, "RightN Pid not_found in nameservice, Fahre runter"), exit(kill);
                _ -> continue
             end
    end,
    receive {setpm, NewMi} -> logge_status(GGTProName, io_lib:format("Pm bekommen: ~p",[NewMi])) end,
    
    receive_loop({GGTProName, NewMi, {LeftPid, RightPid}, {0, false, false, erlang:timestamp()}}, 
                    {ArbeitsZeit, TermZeit, Quota, NsPid, KoPid}).

lookup_name_at_ns(Name, NsPid) ->
    NsPid ! {self(), {lookup, Name}},
    receive
        {pin, Pid} -> Pid;
        not_found -> not_found
    end.



receive_loop(InstanceVariables, GlobalVariables) -> 
    {GGTProName, Mi, Neighbors, {VoteCount, TermFlag, VoteFlag, LastContact}} = InstanceVariables,
	{ArbeitsZeit, TermZeit, Quota, NsPid, KoPid} = GlobalVariables,
	receive
        {setpm, NewMi} -> 				
			logge_status(GGTProName, io_lib:format("Starte neue Berechnung mit Mi = ~p", [NewMi])),
			receive_loop({GGTProName, NewMi, Neighbors, {0, false, false, erlang:timestamp()}}, GlobalVariables);
		 
		{sendy, Y} ->
			NewMi = calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid),
			timer:sleep(timer:seconds(ArbeitsZeit)),
			receive_loop({GGTProName, NewMi, Neighbors, {0, TermFlag, false, erlang:timestamp()}}, GlobalVariables);
			
		{From, tellmi} ->
			tellmi(From, Mi),			
			logge_status(GGTProName, io_lib:format("Mi an ~p gesendet.", [From])),
			receive_loop(InstanceVariables, GlobalVariables);
			
		{From, pingGGT} -> 
			pongGGT(From, GGTProName),		
			logge_status(GGTProName, io_lib:format("Ping von ~p erhalten.", [From])),
			receive_loop(InstanceVariables, GlobalVariables);
		
		{voteYes, OtherGGTProName} ->                  
            logge_status_vote_yes(GGTProName, OtherGGTProName),
            {NewTerm, NewVoteCount, BriefFlag} = vote_yes(TermFlag, Quota, VoteCount),
			case BriefFlag of
				true ->
                    term(KoPid, GGTProName, Mi, Quota);
                false ->
                    donothing			
			end,			
			receive_loop({GGTProName, Mi, Neighbors, {NewVoteCount, NewTerm, VoteFlag, LastContact}}, GlobalVariables);
		
		{InitiatorPid, {vote, InitiatorName}} ->   
            vote(InitiatorPid, InitiatorName, GGTProName, TermFlag, TermZeit, LastContact),			
			receive_loop(InstanceVariables, GlobalVariables);
			
		kill -> 
			kill(GGTProName, NsPid);
        
        Any ->  logge_status(GGTProName, io_lib:format("Got unkonwn: ~p", [Any])),
                receive_loop(InstanceVariables, GlobalVariables)

        after timer:seconds(TermZeit) ->  
			case VoteFlag of
                true ->
                    receive_loop(InstanceVariables, GlobalVariables);
				false ->
					case TermFlag of
                        true ->
                            receive_loop(InstanceVariables, GlobalVariables);
						false ->
							start_vote(GGTProName, Mi, NsPid),				
							receive_loop({GGTProName, Mi, Neighbors, {0, TermFlag, true, LastContact}}, GlobalVariables)
					end
			end		
			
    end.


term(KoPid, GGTProName, Mi, Quota) ->
    KoPid ! {self(), briefterm, {GGTProName, Mi, vsutil:now2string(erlang:timestamp())}},
    logge_status(GGTProName, io_lib:format("Genuegend Votes erhalten (~p), Terminiere mit ~p (Mi)", [Quota, Mi])).


vote(InitiatorPid, InitiatorName, GGTProName, TermFlag, TermZeit, LastContact) ->
    case TermFlag of 
        true -> 
            InitiatorPid ! {voteYes, GGTProName},
            logge_status_vote(GGTProName, InitiatorName, TermFlag);
        false ->
            {_, LastSecs, _} = LastContact,
            {_, NowSecs, _} = erlang:timestamp(),
            case (NowSecs - LastSecs) >= (TermZeit/2) of
                true -> 
                    InitiatorPid ! {voteYes, GGTProName},
                    logge_status_vote(GGTProName, InitiatorName, true);
                false ->
                    logge_status_vote(GGTProName, InitiatorName, false)
            end			
    end.

calc_and_send_new_mi(Mi, Y, Neighbors, GGTProName, KoPid) ->
    NewMi = calc_new_mi(Mi, Y),
    case NewMi of
        Mi ->   
            logge_status(GGTProName, io_lib:format("Neues Y (~p) ergab kein neues Mi", [Y]));
        _Any -> 
            logge_status(GGTProName, io_lib:format("Neues Y (~p) ergab neues Mi (~p), wird an Nachbarn und Koordinator gesendet", [Y, Mi])),
            send_new_mi(NewMi, Neighbors, GGTProName, KoPid)
    end,
    NewMi.

calc_new_mi(Mi, Y) -> 
    case Mi > Y of
        true -> NewMi = ((Mi - 1) rem Y) + 1;
        false -> NewMi = Mi
    end,
    NewMi.

send_new_mi(NewMi, {LeftN, RightN}, GGTProName, KoPid) ->
    LeftN ! {sendy, NewMi},
    RightN ! {sendy, NewMi},
    KoPid ! {briefmi, {GGTProName, NewMi, vsutil:now2string(erlang:timestamp())}}.

vote_yes(TermFlag, Quota, VoteCount) ->
	NewVoteCount = VoteCount + 1,	
	case TermFlag of
		true ->
			{true, NewVoteCount, false};
		false ->
			case (NewVoteCount >= Quota) of
				true ->
					{true, NewVoteCount, true};
				false ->
					{false, NewVoteCount, false}
			end
	end.

tellmi(ReceiverPid, Mi) ->
    ReceiverPid ! {mi, Mi}.

pongGGT(ReceiverPid, GGTProName) ->
    ReceiverPid ! {pongGGT, GGTProName}.

kill(GGTProName, NsPid) ->
    NsPid ! {self(), {unbind, GGTProName}},
    receive
        ok -> ok
    end,

    case whereis(GGTProName) of
        undefined -> ok; %Only for Test purposes! Because since its in the same process the name is always registered during normal run until unregistered here.
        _Any -> unregister(GGTProName)
    end,
    logge_status(GGTProName, "ist heruntergefahren").

start_vote(GGTProName, Mi, NsPid) ->
    logge_status(GGTProName, lists:flatten(io_lib:format("mit ~p (Mi) wird vote gestartet", [Mi]))),
    NsPid ! {self(), {multicast, vote, GGTProName}}.

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status_vote(GGTProName1, GGTProName2, WillRespond) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("Vote von ~p, voteYes gesendet: ~p", [GGTProName2, WillRespond]))).

logge_status_vote_yes(GGTProName1, GGTProName2) ->
    logge_status(GGTProName1, lists:flatten(io_lib:format("VoteYes von ~p", [GGTProName2]))).

logge_status(GGTProName, Inhalt) ->
    LogDateiName = lists:flatten(io_lib:format("~p.log", [GGTProName])),
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s.\n", [GGTProName, AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDateiName, LogNachricht).