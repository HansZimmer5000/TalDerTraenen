-module(client).

% API
-export([
        start/0,

        frage_nach_neuer_nnr/1,
        erstelle_nachricht/2,
        erstelle_nachrichten_text/1,
        pruefe_nnr_und_sende_nachricht/3,
        kalkuliere_neuen_intervall_sek/1,

        frage_nach_neuer_nachricht/1,
        empfangene_nachricht_ist_von_meinem_redakteur/2,
        logge_empfangene_nachricht/2,

        zufalls_boolean/0,
        element_ist_in_liste/2,
        nachricht_zu_text/1,
        neue_nnr_einfuegen/2
        ]).

% KONSTANTEN
-define(MIN_INTERVALL_ZEIT_SEK, 2.0).
-define(LOG_DATEI_NAME, "client.log").
-define(CONFIG_FILENAME, "client.cfg").
-define(CLIENT_ANZAHL, hohle_wert_aus_config_mit_key(clientAnzahl)).
-define(SERVERNAME, hohle_wert_aus_config_mit_key(servername)).
-define(SERVERNODE, hohle_wert_aus_config_mit_key(servernode)).
-define(SERVER, {?SERVERNAME, ?SERVERNODE}).

% INIT
start() ->
    util:logging(?LOG_DATEI_NAME, "Client:start wird ausgefuehrt"),
    %TODO: Start N Clients
    ClientPid = spawn(fun() -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, []) end),
    register(list_to_atom(lists:concat(["name",0])), ClientPid),
    util:logging(?LOG_DATEI_NAME, "Client gestartet"),
    ClientPid.

% LOOPS
redakteur_loop(Intervall, GeschriebeneNNRListe) -> 
    logge_status("Beginne redakteur_loop"),
    NNR = frage_nach_neuer_nnr(?SERVER),
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = erstelle_nachricht(NNR, TS),
    logge_nachricht_status(Nachricht, "erstellt"),

    NeueGeschriebeneNNRListe = lists:flatten([NNR, GeschriebeneNNRListe]),
    pruefe_nnr_und_sende_nachricht(?SERVER, Nachricht, NeueGeschriebeneNNRListe),
    NeuerIntervall = kalkuliere_neuen_intervall_sek(Intervall),
    logge_nachricht_status(Nachricht, "abgearbeitet"),

    case length(NeueGeschriebeneNNRListe) of
        5 -> leser_loop(NeueGeschriebeneNNRListe);
        _Any -> redakteur_loop(NeuerIntervall, NeueGeschriebeneNNRListe)
    end.


leser_loop(GeschriebeneNNRListe) ->
    logge_status(io_lib:format("Beginne leser_loop mit NNRListe: ~p" , [GeschriebeneNNRListe])),
    NeueNachricht = frage_nach_neuer_nachricht(?SERVER),
    case NeueNachricht of
        [] -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, []);
        _Any -> logge_empfangene_nachricht(NeueNachricht, GeschriebeneNNRListe),
                leser_loop(GeschriebeneNNRListe)
    end.


% FUNKTIONEN
frage_nach_neuer_nnr(Server) ->
    Server ! {self(), getmsgid},
    logge_status("Warte auf NNR"),
    receive
        {nid, NNR} -> logge_status(io_lib:format("NNR ~w bekommen", [NNR]))
    end,
    NNR.

erstelle_nachricht(NNR, ErstellungsTS) ->
    Textnachricht = erstelle_nachrichten_text(ErstellungsTS),
    Nachricht = [NNR, Textnachricht, ErstellungsTS],
    Nachricht.

erstelle_nachrichten_text(ErstellungsTS) -> 
    Hostname = hostname1,
    Praktikumsgruppe = gruppe1,
    Teamnummer = team1,
    Nachricht = io_lib:format("~p, ~p, ~p, ~s", [Hostname, Praktikumsgruppe, Teamnummer, ErstellungsTS]),
    Nachricht.


neue_nnr_einfuegen(NNR, []) -> [NNR];
neue_nnr_einfuegen(NNR, NNRListe) ->
    NeueNNRListe = lists:flatten([NNR, NNRListe]),
    NeueNNRListe.


pruefe_nnr_und_sende_nachricht(Server, Nachricht, NNRListe) ->
    Anzahl_Erstellter_Nachrichten = length(NNRListe),
    case Anzahl_Erstellter_Nachrichten of
        5 -> logge_nachricht_status(Nachricht, "vergessen zu senden");
        _Any -> Server ! {dropmessage, Nachricht},
                logge_nachricht_status(Nachricht, "gesendet")
    end.


frage_nach_neuer_nachricht(Server) -> 
    Server ! {self(), getmessages},
    logge_status("Warte auf Nachricht"),

    receive
        {reply, Nachricht, TerminatedFlag} -> ok
    end,
    logge_nachricht_status(Nachricht, io_lib:format("erhalten mit TerminatedFlag = ~p", [TerminatedFlag])),

    case TerminatedFlag of
        true -> Ergebnis = [];
        false -> Ergebnis = Nachricht
    end,
    Ergebnis.



kalkuliere_neuen_intervall_sek(Intervall) ->
    case is_float(Intervall) of
        true -> case zufalls_boolean() of
                        true -> Faktor = 1.5;
                        _Else -> Faktor =  0.5
                end,
                NeuerIntervall = Intervall * Faktor,
                case NeuerIntervall of
                    NeuerIntervall when NeuerIntervall < ?MIN_INTERVALL_ZEIT_SEK -> ?MIN_INTERVALL_ZEIT_SEK;
                    NeuerIntervall when NeuerIntervall >= ?MIN_INTERVALL_ZEIT_SEK -> NeuerIntervall
                end;
        false -> ?MIN_INTERVALL_ZEIT_SEK
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


element_ist_in_liste(_Elem, []) -> false;
element_ist_in_liste(Elem, [Elem | _Rest]) -> true;
element_ist_in_liste(Elem, [_Head | Rest]) ->
    element_ist_in_liste(Elem, Rest).


hohle_wert_aus_config_mit_key(Key) ->
    logge_status(io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).

logge_nachricht_status(Nachricht, Status) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht).

logge_empfangene_nachricht(Nachricht, NummernListe) ->
    [_NNR, Textnachricht | _Rest] = Nachricht, 
    case empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NummernListe) of
        true -> logge_status(io_lib:format("Empfangene Nachricht ~s ist von meinem Redakteur", [Textnachricht]));
        false -> logge_status(io_lib:format("Empfangene Nachricht ~s", [Textnachricht]))
    end.