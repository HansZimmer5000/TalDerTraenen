-module(client).

% API
-export([
        start/0,

        frage_nach_neuer_nnr/2,
        erstelle_nachricht/2,
        erstelle_nachrichten_text/1,
        pruefe_nnr_und_sende_nachricht/4,
        kalkuliere_neuen_intervall_sek/1,

        frage_nach_neuer_nachricht/2,
        empfangene_nachricht_ist_von_meinem_redakteur/2,
        logge_empfangene_nachricht/3,
        erstelle_empfangene_nachricht_logtext/2,

        zufalls_boolean/0,
        element_ist_in_liste/2,
        nachricht_zu_text/1,
        neue_nnr_einfuegen/2
        ]).

% KONSTANTEN
-define(CONFIG_FILENAME, "client.cfg").
-define(MIN_INTERVALL_ZEIT_SEK, 2).
-define(CLIENT_ANZAHL, hole_wert_aus_config_mit_key(clients)).
-define(LIFETIME, hole_wert_aus_config_mit_key(lifetime)).
-define(SERVERNAME, hole_wert_aus_config_mit_key(servername)).
-define(SERVERNODE, hole_wert_aus_config_mit_key(servernode)).
-define(SERVER, {?SERVERNAME, ?SERVERNODE}).

%------------------------------------------------------------------------------------------------------
%																	>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start() ->
    net_adm:ping(?SERVERNODE),

    LogDatei = erstelle_log_datei_name(0),
    logge_status(io_lib:format("client mit PID ~p gestartet", [self()]), LogDatei),
    ClientPidList = start_all_clients(?CLIENT_ANZAHL, []),
    timer:sleep(timer:seconds(?LIFETIME)),
    kill_all_clients(ClientPidList, LogDatei).

start_all_clients(0, AlteClientPidList) -> AlteClientPidList;
start_all_clients(AktuelleClientnummer, AlteClientPidList) ->
    ClientPid = start_client_node(AktuelleClientnummer),
    NeueClientPidList = [ClientPid | AlteClientPidList],
    start_all_clients(AktuelleClientnummer - 1, NeueClientPidList).

start_client_node(Clientnummer) ->
    Clientname = lists:concat(["client", Clientnummer]),
    LogDatei = erstelle_log_datei_name(Clientnummer),

    ClientPid = spawn(fun() -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, [], LogDatei) end),
    register(list_to_atom(Clientname), ClientPid),

    logge_status(lists:concat([Clientname, " mit PID ", io_lib:format("~p", [ClientPid]), " gestartet"]), LogDatei),
    ClientPid.

%------------------------------------------------------------------------------------------------------
%																	>>LOOPS<<
%------------------------------------------------------------------------------------------------------
redakteur_loop(Intervall, GeschriebeneNNRListe, LogDatei) -> 
    logge_status("Beginne redakteur_loop", LogDatei),
    NNR = frage_nach_neuer_nnr(?SERVER, LogDatei),
    TS = erlang:timestamp(),
    Nachricht = erstelle_nachricht(NNR, TS),
    logge_nachricht_status(Nachricht, "erstellt", LogDatei),

    timer:sleep(timer:seconds(Intervall)),
    logge_status(io_lib:format("Intervall von ~p Sek. vorbei", [Intervall]), LogDatei),

    NeueGeschriebeneNNRListe = lists:flatten([NNR, GeschriebeneNNRListe]),
    pruefe_nnr_und_sende_nachricht(?SERVER, Nachricht, NeueGeschriebeneNNRListe, LogDatei),
    NeuerIntervall = kalkuliere_neuen_intervall_sek(Intervall),
    logge_nachricht_status(Nachricht, "abgearbeitet", LogDatei),

    case length(NeueGeschriebeneNNRListe) of
        5 -> leser_loop(NeuerIntervall, NeueGeschriebeneNNRListe, LogDatei);
        _Any -> redakteur_loop(NeuerIntervall, NeueGeschriebeneNNRListe, LogDatei)
    end.


leser_loop(Intervall, GeschriebeneNNRListe, LogDatei) ->
    logge_status(io_lib:format("Beginne leser_loop mit NNRListe: ~w" , [GeschriebeneNNRListe]), LogDatei),
    NeueNachricht = frage_nach_neuer_nachricht(?SERVER, LogDatei),
    case NeueNachricht of
        [] -> redakteur_loop(Intervall, [], LogDatei);
        _Any -> logge_empfangene_nachricht(NeueNachricht, GeschriebeneNNRListe, LogDatei),
                leser_loop(Intervall, GeschriebeneNNRListe, LogDatei)
    end.


%------------------------------------------------------------------------------------------------------
%																	>>EIGENTLICHE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------

frage_nach_neuer_nnr(Server, LogDatei) ->
    Server ! {self(), getmsgid},
    logge_status("Warte auf NNR", LogDatei),
    receive
        {nid, NNR} -> 
            logge_status(io_lib:format("NNR ~w bekommen", [NNR]), LogDatei),
            NNR;
        {kill} -> exit("Kill Befehl vom Main Client")
    end.

erstelle_nachricht(NNR, ErstellungsTS) ->
    Textnachricht = erstelle_nachrichten_text(ErstellungsTS),
    Nachricht = [NNR, Textnachricht, ErstellungsTS],
    Nachricht.

erstelle_nachrichten_text(ErstellungsTS) -> 
    Hostname = hole_wert_aus_config_mit_key(hostname),
    Praktikumsgruppe = hole_wert_aus_config_mit_key(praktikumsgruppe),
    Teamnummer = hole_wert_aus_config_mit_key(teamnummer),
    Nachricht = io_lib:format("~p, ~p, ~p, ~s", [Hostname, Praktikumsgruppe, Teamnummer, vsutil:now2string(ErstellungsTS)]),
    lists:flatten(Nachricht).


neue_nnr_einfuegen(NNR, []) -> [NNR];
neue_nnr_einfuegen(NNR, NNRListe) ->
    NeueNNRListe = lists:flatten([NNR, NNRListe]),
    NeueNNRListe.


pruefe_nnr_und_sende_nachricht(Server, Nachricht, NNRListe, LogDatei) ->
    Anzahl_Erstellter_Nachrichten = length(NNRListe),
    case Anzahl_Erstellter_Nachrichten of
        5 -> logge_nachricht_status(Nachricht, "vergessen zu senden", LogDatei);
        _Any -> Server ! {dropmessage, Nachricht},
                logge_nachricht_status(Nachricht, "gesendet", LogDatei)
    end.


frage_nach_neuer_nachricht(Server, LogDatei) -> 
    Server ! {self(), getmessages},
    logge_status("Warte auf Nachricht", LogDatei),

    receive
        {reply, Nachricht, TerminatedFlag} -> 
            ok,
            logge_nachricht_status(Nachricht, io_lib:format("erhalten mit TerminatedFlag = ~p", [TerminatedFlag]), LogDatei),
            case TerminatedFlag of
                true -> Ergebnis = [];
                false -> Ergebnis = Nachricht
            end,
            Ergebnis;
        {kill} -> exit("Kill Befehl vom Main Client")
    end.




kalkuliere_neuen_intervall_sek(Intervall) ->
    case zufalls_boolean() of
                true -> Faktor = 1.5;
                _Else -> Faktor =  0.5
        end,
        NeuerIntervall = Intervall * Faktor,
        case NeuerIntervall of
            NeuerIntervall when NeuerIntervall < ?MIN_INTERVALL_ZEIT_SEK -> ?MIN_INTERVALL_ZEIT_SEK;
            NeuerIntervall when NeuerIntervall >= ?MIN_INTERVALL_ZEIT_SEK -> round(NeuerIntervall)
    end.


zufalls_boolean() ->
    rand:uniform() > 0.5.


nachricht_zu_text(Nachricht) ->
    [NNR | Rest] = Nachricht,
    Akku = io_lib:format("~w", [NNR]),
    nachricht_zu_text_(Rest, Akku).

nachricht_zu_text_([], Akku) -> Akku;
nachricht_zu_text_([NachrichtHead | NachrichtRest], Akku) ->
    NeuerAkku = lists:flatten(io_lib:format("~s, ~s", [Akku, NachrichtHead])),
    nachricht_zu_text_(NachrichtRest, NeuerAkku).


empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NummernListe) ->
    [NNR | _Tail] = Nachricht,
    element_ist_in_liste(NNR, NummernListe).

ts_ist_aus_der_zukunft(DLQoutTS, JetztTS) ->
    vsutil:validTS(DLQoutTS),
    vsutil:validTS(JetztTS),
    NachrichtIstAusDerZukunft = vsutil:lessTS(JetztTS, DLQoutTS),
    NachrichtIstAusDerZukunft.

erstelle_diffts_string(TS1, TS2) ->
    vsutil:validTS(TS1),
    vsutil:validTS(TS2),
    FullDiffTSString = vsutil:now2string(vsutil:diffTS(TS2, TS1)),
    DiffTSString = lists:sublist(FullDiffTSString, 10, 9),
    DiffTSString.





element_ist_in_liste(_Elem, []) -> false;
element_ist_in_liste(Elem, [Elem | _Rest]) -> true;
element_ist_in_liste(Elem, [_Head | Rest]) ->
    element_ist_in_liste(Elem, Rest).


kill_all_clients([], LogDatei) -> 
    timer:sleep(timer:seconds(1)),
	logge_status("Alle Clients wurden getoetet", LogDatei);
kill_all_clients([Client|RestClients], LogDatei) ->
	%exit(HeadClient,kill),
	Client ! {kill},
	logge_status(io_lib:format("Der Client ~p wurde zur Selbstzerstoerung ueberredet", [Client]), LogDatei),
	kill_all_clients(RestClients, LogDatei).

%------------------------------------------------------------------------------------------------------
%																	>>GENERELLE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
erstelle_log_datei_name(Clientnummer) ->
    LogDatei = "client" ++ io_lib:format("~p",[Clientnummer]) ++ ".log",
    LogDatei.

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = erlang:timestamp(),
    LogNachricht = io_lib:format("~p ~s.\n", [vsutil:now2string(AktuelleZeit), Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).

logge_nachricht_status(Nachricht, Status, LogDatei) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht, LogDatei).

logge_empfangene_nachricht(Nachricht, NummernListe, LogDatei) ->
    LogText = erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    logge_status(LogText, LogDatei).

erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe) ->
    [_NNR, Textnachricht, _TSCOut, _TSHIn, _TSDIn, DLQoutTS] = Nachricht, 
    
    JetztTS = erlang:timestamp(),
    NachrichtIstAusDerZukunft = ts_ist_aus_der_zukunft(DLQoutTS, JetztTS),
    NachrichtIstVonMeinemRedakteur = empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NummernListe),

    case NachrichtIstAusDerZukunft of
        true -> 
            DiffTSString = erstelle_diffts_string(DLQoutTS, JetztTS),
            LogZusatz1 = io_lib:format("ist um ~p aus der Zukunft", [DiffTSString]);
        false -> LogZusatz1 = ""
    end,

    case NachrichtIstVonMeinemRedakteur of
        true -> LogZusatz2 = "ist von meinem Redakteur";
        false -> LogZusatz2 = ""
    end,

    io_lib:format("Empfangene Nachricht '~s' ~s ~s", [Textnachricht, LogZusatz1, LogZusatz2]).