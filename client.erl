-module(client).

% API
-export([
        erstelle_nachrichten_text/0,
        kalkuliere_neuen_intervall_sek/1,
        zufalls_boolean/0,
        element_ist_in_liste/2,
        empfangene_nachricht_ist_von_meinem_redakteur/2,
        logge_dass_nicht_senden_von_nachricht/1,
        logge_empfangene_nachricht/2,
        frage_nach_neuer_nnr/0,
        nachricht_zu_text/1,
        erstelle_nachricht/2,
        neue_nnr_einfuegen/2,
        start/0
        ]).

% KONSTANTEN
-define(MIN_INTERVALL_ZEIT_SEK, 2.0).
-define(LOG_DATEI_NAME, "client.log").
-define(CONFIG_FILENAME, "client.cfg").
-define(CLIENT_ANZAHL, hohle_wert_aus_config_mit_key(clientAnzahl)).
-define(SERVERNAME, hohle_wert_aus_config_mit_key(servername)).
-define(SERVERNODE, hohle_wert_aus_config_mit_key(servernode)).

% INIT
start() ->
    %TODO: Start N Clients
    ClientPid = spawn(fun() -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, []) end),
    register(list_to_atom(lists:concat(["name",0])), ClientPid),
    ClientPid.

% LOOPS
redakteur_loop(Intervall, GeschriebeneNNRListe) -> 
    io:fwrite("redakteur"),
    NNR = frage_nach_neuer_nachricht(),
    TS = werkzeug:now2string(erlang:timestamp()),
    Nachricht = erstelle_nachricht(NNR, TS),
    NeueGeschriebeneNNRListe = lists:flatten([NNR, GeschriebeneNNRListe]),
    sende_nachricht(Nachricht, NeueGeschriebeneNNRListe),
    NeuerIntervall = kalkuliere_neuen_intervall_sek(Intervall),
    case length(NeueGeschriebeneNNRListe) of
        5 -> leser_loop(NeueGeschriebeneNNRListe);
        _Any -> redakteur_loop(NeuerIntervall, NeueGeschriebeneNNRListe)
    end.


leser_loop(GeschriebeneNNRListe) ->
    io:fwrite("leser"),
    NeueNachricht = frage_nach_neuer_nachricht(),
    case NeueNachricht of
        [] -> redakteur_loop(?MIN_INTERVALL_ZEIT_SEK, []);
        _Any -> logge_empfangene_nachricht(NeueNachricht, GeschriebeneNNRListe),
                leser_loop(GeschriebeneNNRListe)
    end.


% FUNKTIONEN
frage_nach_neuer_nnr()->
    SERVER = {?SERVERNAME, ?SERVERNODE},
    SERVER ! {self(), getmsgid},
    receive
        {nid, NNR} -> io:fwrite("Habe NNR ~w bekommen.", [NNR]);
        _Any -> io:fwrite("Habe keine NNR bekommen."),
                NNR = 0
    end,
    NNR.

erstelle_nachricht(0, _TS) -> [];
erstelle_nachricht(NNR, ErstellungsTS) ->
    Textnachricht = erstelle_nachrichten_text(),
    Nachricht = [NNR, Textnachricht, ErstellungsTS],
    Nachricht.

erstelle_nachrichten_text() -> 
    Hostname = hostname1,
    Praktikumsgruppe = gruppe1,
    Teamnummer = team1,
    NowTs = werkzeug:now2string(erlang:timestamp()),
    Nachricht = io_lib:format("~p, ~p, ~p, ~s", [Hostname, Praktikumsgruppe, Teamnummer, NowTs]),
    NachrichteFlatten = lists:flatten(Nachricht),
    NachrichteFlatten.


neue_nnr_einfuegen(NNR, []) -> [NNR];
neue_nnr_einfuegen(NNR, NNRListe) ->
    NeueNNRListe = lists:flatten([NNR, NNRListe]),
    NeueNNRListe.


sende_nachricht(Nachricht, NNRListe) ->
    Anzahl_Erstellter_Nachrichten = length(NNRListe),
    case Anzahl_Erstellter_Nachrichten of
        5 -> logge_dass_nicht_senden_von_nachricht(Nachricht);
        _Any -> Server = {?SERVERNAME, ?SERVERNODE},
                Server ! {dropmessage, Nachricht},
                logge_dass_senden_von_nachricht(Nachricht)
    end.


frage_nach_neuer_nachricht() -> 
    Server = {?SERVERNAME, ?SERVERNODE},
    Server ! {self(), getmessages},
    receive
        {reply, _Nachricht, true} -> Ergebnis = [];
        {reply, Nachricht, false} -> Ergebnis = Nachricht;
        _Any -> io:fwrite("Keine Nachricht bekommen."),
                Ergebnis = []
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

hohle_text_aus_nachricht(Nachricht) ->
    case Nachricht of
        {NNR, Text, TSclientout, TShbqin, TSdlqin, TSdlqout} -> Text;
        {NNR, Text, TSclientout, TShbqin, TSdlqin} -> Text;
        {NNR, Text, TSclientout, TShbqin} -> Text;
        {NNR, Text, TSclientout} -> Text
    end.

logge_empfangene_nachricht(Nachricht, NummernListe) ->
    [_NNR, Textnachricht | _Rest] = Nachricht, %Nachricht ist keine Liste! Sondern ein Tupel mit N Stellen!! (Siehe Anzahl TS)
    case empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NummernListe) of
        true ->
            NeueTextnachricht = lists:flatten(io_lib:format("~s ~s", ["Ist von meinem Redakteur", Textnachricht])),
            werkzeug:logging(?LOG_DATEI_NAME, NeueTextnachricht);
        false -> werkzeug:logging(?LOG_DATEI_NAME, Textnachricht)
    end,
    werkzeug:logging(?LOG_DATEI_NAME, "\n").


logge_dass_nicht_senden_von_nachricht(Nachricht) -> 
    [NNR | _Rest] = Nachricht,
    AktuelleZeit = werkzeug:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s", [NNR, AktuelleZeit, "vergessen zu senden."]),
    LogNachrichtenFlatten = lists:flatten(LogNachricht),
    werkzeug:logging(?LOG_DATEI_NAME, LogNachrichtenFlatten).


logge_dass_senden_von_nachricht(Nachricht) -> 
    [NNR | _Rest] = Nachricht,
    AktuelleZeit = werkzeug:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~p ~s", [NNR, AktuelleZeit, "gesendet."]),
    LogNachrichtenFlatten = lists:flatten(LogNachricht),
    werkzeug:logging(?LOG_DATEI_NAME, LogNachrichtenFlatten).


nachricht_zu_text([]) -> "";
nachricht_zu_text(Nachricht) ->
    [NNR, Textnachricht | Timestamps] = Nachricht,
    Akku = lists:flatten(io_lib:format("~w, ~s", [NNR, Textnachricht])),
    nachricht_zu_text_(Timestamps, Akku).


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
    %log_status(extractValueFromConfig,io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = werkzeug:get_config_value(Key, ConfigListe),
    Value.