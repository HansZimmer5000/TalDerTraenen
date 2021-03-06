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
        element_ist_in_liste/2
        ]).

% KONSTANTEN
-define(CONFIG_FILENAME, "client.cfg").
-define(MIN_INTERVALL_ZEIT_SEK, 2).
-define(CLIENT_ANZAHL, hole_wert_aus_config_mit_key(clients)).
-define(LIFETIME, hole_wert_aus_config_mit_key(lifetime)).
-define(SERVERNAME, hole_wert_aus_config_mit_key(servername)).
-define(SERVERNODE, hole_wert_aus_config_mit_key(servernode)).
-define(SERVER, {?SERVERNAME, ?SERVERNODE}).

-define(HOSTNAME, hostname1).
-define(PRAKTIKUMSGRUPPE, gruppe2).
-define(TEAMNUMMER, team6).

%------------------------------------------------------------------------------------------------------
%										>>START / INIT<<
%------------------------------------------------------------------------------------------------------
start() ->
    net_adm:ping(?SERVERNODE),

    LogDatei = erstelle_log_datei_name(0),
    logge_status(io_lib:format("client mit PID ~p gestartet", [self()]), LogDatei),
    ClientPidList = start_all_clients(?CLIENT_ANZAHL, []),
    timer:sleep(timer:seconds(?LIFETIME)),
    kill_all_clients(ClientPidList, LogDatei).

start_all_clients(0, AlteClientPidList) -> 
    AlteClientPidList;
start_all_clients(AktuelleClientnummer, AlteClientPidList) ->
    ClientPid = start_client_node(AktuelleClientnummer),
    NeueClientPidList = [ClientPid | AlteClientPidList],
    start_all_clients(AktuelleClientnummer - 1, NeueClientPidList).

start_client_node(Clientnummer) ->
    Clientname = lists:concat(["client", Clientnummer]),
    LogDatei = erstelle_log_datei_name(Clientnummer),

    ClientPid = spawn(fun() -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, [], LogDatei) end),

    logge_status(lists:concat([Clientname, " mit PID ", io_lib:format("~p", [ClientPid]), " gestartet"]), LogDatei),
    ClientPid.

%------------------------------------------------------------------------------------------------------
%								>>LOOPS<<
%------------------------------------------------------------------------------------------------------
% Der redakteur_loop ist dafür da, dass der Client Nachrichten an den Server schreibt.
% Wie im Entwurf beschrieben versendet der Client 5 Nachrichten an den Server, 
% die letzte Nachricht vergisst der Client zu senden und wechelt zum leser_loop.
% Die geschriebenen NNRn werden in einer Liste hinterlegt.
redakteur_loop(Intervall, GeschriebeneNNRListe, LogDatei) -> 
    logge_status("Beginne redakteur_loop", LogDatei),

    NNR = frage_nach_neuer_nnr(?SERVER, LogDatei),
    TS = erlang:timestamp(),
    Nachricht = erstelle_nachricht(NNR, TS),
    logge_nachricht_status(Nachricht, "erstellt", LogDatei),

    timer:sleep(timer:seconds(Intervall)),
    logge_status(io_lib:format("Intervall von ~p Sek. vorbei", [Intervall]), LogDatei),

    NeueGeschriebeneNNRListe = [NNR] ++ GeschriebeneNNRListe,
    pruefe_nnr_und_sende_nachricht(?SERVER, Nachricht, NeueGeschriebeneNNRListe, LogDatei),
    NeuerIntervall = kalkuliere_neuen_intervall_sek(Intervall),
    logge_nachricht_status(Nachricht, "abgearbeitet", LogDatei),

    case length(NeueGeschriebeneNNRListe) of
        5 -> leser_loop(NeuerIntervall, NeueGeschriebeneNNRListe, LogDatei);
        _Any -> redakteur_loop(NeuerIntervall, NeueGeschriebeneNNRListe, LogDatei)
    end.

% Der leser_loop fragt den Server nach neuen Nachrichten und loggt diese, bis keine mehr vorhanden sind.
% Sind keine Nachrichten mehr vorhanden, wechselt dieser zum redakteur_loop.
leser_loop(Intervall, GeschriebeneNNRListe, LogDatei) ->
    logge_status(io_lib:format("Beginne leser_loop mit NNRListe: ~w" , [GeschriebeneNNRListe]), LogDatei),
    NeueNachricht = frage_nach_neuer_nachricht(?SERVER, LogDatei),
    case NeueNachricht of
        [] -> redakteur_loop(Intervall, [], LogDatei);
        _Any -> logge_empfangene_nachricht(NeueNachricht, GeschriebeneNNRListe, LogDatei),
                leser_loop(Intervall, GeschriebeneNNRListe, LogDatei)
    end.


%------------------------------------------------------------------------------------------------------
%								>>EIGENTLICHE FUNKTIONEN<<
%------------------------------------------------------------------------------------------------------
% Wie im Entwurf beschrieben, fragt der Client den Server nach der naechsten NNR.
frage_nach_neuer_nnr(Server, LogDatei) ->
    Server ! {self(), getmsgid},
    logge_status("Warte auf NNR", LogDatei),
    receive
        {nid, NNR} -> 
            logge_status(io_lib:format("NNR ~w bekommen", [NNR]), LogDatei),
            NNR
    end.

% Diese Funktion erstellt die zu sendende Nachricht.
% Die Nachricht beinhaltet [NNR, Textnachricht, ts]
erstelle_nachricht(NNR, ErstellungsTS) ->
    Textnachricht = erstelle_nachrichten_text(ErstellungsTS),
    Nachricht = [NNR, Textnachricht, ErstellungsTS],
    Nachricht.

% Diese Funktion erstellt die Textnachricht in der Nachricht.
% Die Nachricht beinhaltet "Hostname, Praktikumsgruppe, TEAMNUMMER, ts"
erstelle_nachrichten_text(ErstellungsTS) -> 
    Hostname = ?HOSTNAME,
    Praktikumsgruppe = ?PRAKTIKUMSGRUPPE,
    Teamnummer = ?TEAMNUMMER,
    Nachricht = io_lib:format("~p, ~p, ~p, ~s", [Hostname, Praktikumsgruppe, Teamnummer, vsutil:now2string(ErstellungsTS)]),
    lists:flatten(Nachricht).

% Diese Funktion prüft, ob es die 5te Nachricht ist, falls ja vergisst er die Nachricht zu senden.
% Falls nicht, wird die Nachricht an den Server versendet.
pruefe_nnr_und_sende_nachricht(Server, Nachricht, NNRListe, LogDatei) ->
    Anzahl_Erstellter_Nachrichten = length(NNRListe),
    case Anzahl_Erstellter_Nachrichten of
        5 -> logge_nachricht_status(Nachricht, "vergessen zu senden", LogDatei);
        _Any -> Server ! {dropmessage, Nachricht},
                logge_nachricht_status(Nachricht, "gesendet", LogDatei)
    end.

% Wie im Entwurf beschrieben fragt der Client den Server, ob es noch Nachrichten zum lesen gibt.
% Die Antwort vom Server beinhaltet einen Flag, ist dieser true gibt es keine Nachrichten mehr zu lesen.
% Es wird eine leere Liste oder die Nachricht zurückgegeben.
frage_nach_neuer_nachricht(Server, LogDatei) -> 
    Server ! {self(), getmessages},
    logge_status("Warte auf Nachricht", LogDatei),
	
    receive
        {reply, Nachricht, TerminatedFlag} -> 
            ok,
            logge_nachricht_status(Nachricht, io_lib:format("erhalten mit TerminatedFlag = ~p", [TerminatedFlag]), LogDatei),
            case TerminatedFlag of
                true -> 	Ergebnis = [];
                false -> 	Ergebnis = Nachricht
            end,
			Ergebnis
    end.

% Hier wird die Intervallzeit nach der Aufgabenbeschreibung neu kalkuliert.
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

% Diese Funktion generiert ein zufalls Boolean
zufalls_boolean() ->
    rand:uniform() > 0.5.

% Prüft, ob die Nachricht vom eigenen Redakteur ist.
empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NummernListe) ->
    [NNR | _Tail] = Nachricht,
    element_ist_in_liste(NNR, NummernListe).

% Prüft, ob die empfangene Nachricht aus der Zukunft kommt.
% Dabei wird auf das Ausgangszeitstempel der Nachricht geschaut. 
ts_ist_aus_der_zukunft(DLQoutTS, JetztTS) ->
    vsutil:validTS(DLQoutTS),
    vsutil:validTS(JetztTS),
    NachrichtIstAusDerZukunft = vsutil:lessTS(JetztTS, DLQoutTS),
    NachrichtIstAusDerZukunft.

% Gibt den Zeitunterschid zurück. Die Methoden sind aus vsutil, die uns gestellt wurden.
erstelle_diffts_string(TS1, TS2) ->
    vsutil:validTS(TS1),
    vsutil:validTS(TS2),
    FullDiffTSString = vsutil:now2string(vsutil:diffTS(TS2, TS1)),
    DiffTSString = lists:sublist(FullDiffTSString, 10, 9),
    DiffTSString.

% Sucht rekursiv, ob ein Element in der mitgegebenen Liste vorhanden ist. 
element_ist_in_liste(_Elem, []) -> false;
element_ist_in_liste(Elem, [Elem | _Rest]) -> true;
element_ist_in_liste(Elem, [_Head | Rest]) ->
    element_ist_in_liste(Elem, Rest).

% Beendet rekursiv alle aktiven Clients.
kill_all_clients([], LogDatei) -> 
	logge_status("Alle Clients wurden getoetet", LogDatei);
kill_all_clients([Client|RestClients], LogDatei) ->
	exit(Client, kill),
	logge_status(io_lib:format("Der Client ~p wurde zur Selbstzerstoerung ueberredet", [Client]), LogDatei),
	kill_all_clients(RestClients, LogDatei).

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

erstelle_log_datei_name(Clientnummer) ->
    LogDatei = "client" ++ io_lib:format("~p",[Clientnummer]) ++ ".log",
    LogDatei.

% Loggt uebergebene Nachrichten
logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = erlang:timestamp(),
    LogNachricht = io_lib:format("~p ~s.\n", [vsutil:now2string(AktuelleZeit), Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).

% Loggt den mitgegebenen Status zu einer Nachricht.
% Dabei ist der Status frei wählbar.	
logge_nachricht_status(Nachricht, Status, LogDatei) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht, LogDatei).

% loggt empfangene Nachrichten
logge_empfangene_nachricht(Nachricht, NummernListe, LogDatei) ->
    LogText = erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    logge_status(LogText, LogDatei).

% Wie im Entwurf beschrieben wird Geprüft, ob die Nachricht aus der Zukunft kommt und ob die Nachricht
% vom eigenen Redakteur ist, ist dies der Fall, wird diese zusätzlich geloggt.
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

    NeueTextNachricht = lists:concat([Textnachricht, "clientin:", vsutil:now2string(JetztTS)]),
    io_lib:format("Empfangene Nachricht '~s' ~s ~s", [NeueTextNachricht, LogZusatz1, LogZusatz2]).