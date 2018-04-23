-module(koordinator).

-export([
    start/0,
    start/1,

    init_loop/4,
    calc_quote/2,
    create_circle/2,
    set_neighbors/4,
    get_next_to_last_and_last_elem/1,

    calculation_receive_loop/4,
    briefmi/4,
    briefterm/6,
    reset/2,
    calc/3,
    get_pms/2,
    select_random_some_ggtprocesses/1,
    send_pms_to_ggtprocesses/3,
    send_ys_to_ggtprocesses/3,
    get_first_n_elems_of_list/3,
    send_message_to_processname/3,
    prompt/2,
    nudge/2,
    toggle/1,
    kill/2,
    kill_all_ggtprocesses/2,

    ggtpropid_exists/2,
    get_ggtpropid/2
]).

-define(CONFIG_DATEI_NAME, "koordinator.cfg").
-define(LOG_DATEI_NAME, "koordinator.log").

-define(NSPID, global:whereis_name(nameservice)).
-define(NSNODE, hole_wert_aus_config_mit_key(nameservicenode)).
-define(KONAME, hole_wert_aus_config_mit_key(koordinatorname)).

-define(ARBEITSZEIT, hole_wert_aus_config_mit_key(arbeitszeit)).
-define(TERMZEIT, hole_wert_aus_config_mit_key(termzeit)).
-define(QUOTA, hole_wert_aus_config_mit_key(quote)).
-define(GGTPROANZ, hole_wert_aus_config_mit_key(ggtprozessnummer)).
-define(KORRIGIEREN, hole_wert_aus_config_mit_key(korrigieren)).

% Startet den Koordinator und findet die Nameservice PID heraus.
start() ->
    net_adm:ping(?NSNODE), 
    timer:sleep(timer:seconds(2)),
    start(?NSPID).

% Startet den Koordinator mit der Nameservice PID.
start(NsPid) ->
    logge_status("koordinator startet"), 

    net_adm:ping(?NSNODE),    
    register_at_ns(NsPid),

    SteeringValues = {steeringval, ?ARBEITSZEIT, ?TERMZEIT, 0, ?GGTPROANZ},
    GGTProNameList = init_loop(NsPid, SteeringValues, 0, []),
    calculation_receive_loop(GGTProNameList, NsPid, ?KORRIGIEREN, empty).

% Registriert den Koordinator beim Nameservie
register_at_ns(undefined) ->
    logge_status("NsPid unbekannt");
register_at_ns(NsPid) ->
    register(?KONAME, self()),
    NsPid ! {self(), {rebind, ?KONAME, node()}},
    receive
        ok -> logge_status("ist registriert und beim nameservice bekannt")
    end.

% Loop fuer die erste Phase (Init) des Systems bei dem sich Starter und ggT-Prozesse beim Koordinator melden.
% Bis 'step' empfangen, der Kreis erstellt und auf eine Berechnung gewartet wird.
init_loop(NsPid, SteeringValues, CurrentStartersCount, GGTProNameList) ->
    receive
        {AbsenderPid, getsteeringval} ->
            NewStartersCount = CurrentStartersCount + 1,
            {steeringval, Arbeitszeit, Termzeit, _OldQuote, GGTProAnz} = SteeringValues,
            NewQuote = calc_quote(NewStartersCount, GGTProAnz),
            NewSteeringValues = {steeringval, Arbeitszeit, Termzeit, NewQuote, GGTProAnz},

            AbsenderPid ! NewSteeringValues,
            init_loop(NsPid, NewSteeringValues, NewStartersCount, GGTProNameList);
        {hello, GGTProName} ->
            NewGGTProNameList = [GGTProName | GGTProNameList],
            init_loop(NsPid, SteeringValues, CurrentStartersCount, NewGGTProNameList);
        step ->
            logge_status("Es werden keine weiteren ggT-Prozesse beachtet"),
            ShuffledGGTProNameList = util:shuffle(GGTProNameList),
            create_circle(ShuffledGGTProNameList, NsPid),
            GGTProNameList;
        reset ->    
            reset(GGTProNameList, NsPid);
        kill ->     
            kill(GGTProNameList, NsPid)
    end.

% Kalkuliert die Quote von Prozent in eine konkrete Zahl (Integer).
calc_quote(StartersCount, GGTProAnz) -> 
    round(StartersCount * GGTProAnz * 80 / 100).

% Teilt den ggT-Prozessen ihre Nachbarn mit. Und erstellt somit den Kreis.
% Wobei jedoch der Erste und Letzte Eintrag besondert behandelt werden.
create_circle(GGTProNameList, NsPid) ->
    [FirstGGTProName, SecondGGTProName | _RestGGTProNames] = GGTProNameList,
    [NextToLastGGTProName, LastGGTProName] = get_next_to_last_and_last_elem(GGTProNameList),
    set_neighbors(FirstGGTProName, LastGGTProName, SecondGGTProName, NsPid),
    set_neighbors(LastGGTProName, NextToLastGGTProName, FirstGGTProName, NsPid),
    create_circle_(GGTProNameList, NsPid).

create_circle_([_NextToLastGGTProName, _LastGGTProName], _NsPid) -> 
    logge_status("kreis erstellt");
create_circle_([FirstGGTProName, SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid) ->
    set_neighbors(SecondGGTProName, FirstGGTProName, ThirdGGTProName, NsPid),
    create_circle_([SecondGGTProName, ThirdGGTProName | RestGGTProNames], NsPid).

% Startet die Berechnung des WggT. 
calc(WggT, GGTProNameList, NsPid) ->
    PMList = get_pms(WggT, GGTProNameList),
    send_pms_to_ggtprocesses(PMList, GGTProNameList, NsPid),
    logge_status("pms zu ggT-Prozessen gesendet"),
    SelectedGGTProcesses = select_random_some_ggtprocesses(GGTProNameList),
    send_ys_to_ggtprocesses(PMList, SelectedGGTProcesses, NsPid),
    logge_status(lists:flatten(io_lib:format("ys zu ~p ggT-Prozessen gesendet", [length(SelectedGGTProcesses)]))).

% Erstellt mit Hilfe von vsutil:bestimme_mis/2 die PMs der ggT-Prozesse.
get_pms(WggT, GGTProNameList) ->
    GGTProAnz = length(GGTProNameList),
    PMList = vsutil:bestimme_mis(WggT, GGTProAnz),
    PMList.

% Wählt aus allen bekannten ggT-Prozessen zufällig ein Fünftel (mindestens 2) aus.
select_random_some_ggtprocesses(GGTProNameList) ->
    ShuffledGGTProNameList = util:shuffle(GGTProNameList),
    SelectionCount = round(length(GGTProNameList) / 5),
    case SelectionCount < 2 of
        true ->  
            Result = get_first_n_elems_of_list(2, ShuffledGGTProNameList, []);
        false -> 
            Result = get_first_n_elems_of_list(SelectionCount, ShuffledGGTProNameList, [])
    end,
    Result.

% Sendet die PMs an die ggT-Prozesse
send_pms_to_ggtprocesses([], [], _NsPid) -> ok;
send_pms_to_ggtprocesses([HeadPM | RestPMs], [HeadGGTProName | RestGGTProNames], NsPid) ->
    send_message_to_processname({setpm, HeadPM}, HeadGGTProName, NsPid),
    send_pms_to_ggtprocesses(RestPMs, RestGGTProNames, NsPid).

% Sendet die Ys an die vorhin zufällig ausgewählten ggT-Prozesse.
send_ys_to_ggtprocesses(_Ys, [], _NsPid) -> done;
send_ys_to_ggtprocesses([HeadY | RestYs], [HeadGGTProName | RestGGTProNames], NsPid) ->
    send_message_to_processname({sendy, HeadY}, HeadGGTProName, NsPid),
    send_ys_to_ggtprocesses(RestYs, RestGGTProNames, NsPid).

% Holt die ersten N Elemente (in invertierter Reihenfolge) aus einer Liste.
% Beispiel: get_first_n_elems_of_list(2, [1,2,3], []) -> [2,1]
get_first_n_elems_of_list(0, _List, Akku) -> Akku;
get_first_n_elems_of_list(N, [Head | Rest], Akku) ->
    NewAkku = [Head | Akku],
    NewN = N - 1,
    get_first_n_elems_of_list(NewN, Rest, NewAkku).

% Sendet die Nachricht and einen Prozessnamen (der wiederum über den Nameservice zu einer PID aufgelöst wird).
send_message_to_processname(Message, ProName, NsPid) ->
    case ggtpropid_exists(ProName, NsPid) of
        true -> continue;
        false -> throw(ggtpronameUnkownForNs)
    end,
    ProPid = get_ggtpropid(ProName, NsPid),
    ProPid ! Message.

% Teilt dem ersten ggT-Prozess (Middle) mit, welcher Nachbar links und rechts von ihm ist.
set_neighbors(MiddleGGTProName, LeftGGTProName, RightGGTProName, NsPid) ->
    case ggtpropid_exists(MiddleGGTProName, NsPid) of
        true ->     
            continue;
        false ->  
            throw(ggtpronameUnkownForNs)  
    end,
    MiddleGGTProPid = get_ggtpropid(MiddleGGTProName, NsPid),
    MiddleGGTProPid ! {setneighbors, LeftGGTProName, RightGGTProName}.

% Holt aus einer Liste die vorletzten Elemente heraus.
get_next_to_last_and_last_elem([]) ->
    [];
get_next_to_last_and_last_elem([_OneElem]) ->
    get_next_to_last_and_last_elem([]);
get_next_to_last_and_last_elem([NextToLastElem, LastElem]) -> 
    [NextToLastElem, LastElem];
get_next_to_last_and_last_elem([_HeadElem | RestElems]) ->
    get_next_to_last_and_last_elem(RestElems).

% Der Loop wenn auf eine Berechnung gewartet wird (2. Phase), bzw. eine gerade stattfindet (3. Phase). 
calculation_receive_loop(GGTProNameList, NsPid, Korrigieren, LastMinMi) ->
    receive
        {calc, WggT} -> 
            calc(WggT, GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid, ?KORRIGIEREN, WggT); % Hier 'empty' oder WggT als global MinMi setzen.
        {briefmi, {GGTProName, CMi, CZeit}} -> 
            NewMinMi = briefmi(GGTProName, CMi, CZeit, LastMinMi),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren, NewMinMi);
        {AbsenderPid, briefterm, {GGTProName, CMi, CZeit}} -> 
            NewMinMi = briefterm(AbsenderPid, GGTProName, CMi, CZeit, LastMinMi, Korrigieren),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren, NewMinMi); 
        prompt ->   
            prompt(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren, LastMinMi);
        nudge ->    
            nudge(GGTProNameList, NsPid),
            calculation_receive_loop(GGTProNameList, NsPid, Korrigieren, LastMinMi);
        toggle ->   
            NeuesKorrigieren = toggle(Korrigieren),
            calculation_receive_loop(GGTProNameList, NsPid, NeuesKorrigieren, LastMinMi);
        reset ->    
            reset(GGTProNameList, NsPid);
        kill ->     
            kill(GGTProNameList, NsPid)
    end.

% Loggt das eingegangenge briefmi und gibt aktuelles globales minimales Mi zurück (Hier nur wenn MinMi initial = empty war).
briefmi(GGTProName, CMi, CZeit, empty) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, false),
    logge_status(io_lib:format("Neues Globales Mi: ~p (Alt: ~p)", [CMi, empty])),
    CMi;
% Loggt das eingegangenge briefmi und gibt aktuelles globales minimales Mi zurück.
% Da es sich nur um ein Briefmi handelt ist das TerminatedFlag im Logtext false.
briefmi(GGTProName, CMi, CZeit, MinMi) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, false),
    case CMi < MinMi of
        true -> 
            logge_status(io_lib:format("Neues Globales Mi: ~p (Alt: ~p)", [CMi, MinMi])), 
            CMi;
        false -> 
            MinMi
    end.

% Loggt das eingegangene briefterm und gibt aktuelles globales minimales Mi zurück (Hier nur wenn MinMi initial = empty war).
briefterm(_AbsenderPid, GGTProName, CMi, CZeit, empty, _Korrigieren) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, true),
    logge_status(io_lib:format("Neues Globales Mi: ~p (Alt: ~p)", [CMi, empty])), 
    CMi;
% Loggt das eingegangene briefterm und gibt aktuelles globales minimales Mi zurück.
% Da es sich um ein Briefterm handelt ist das TerminatedFlag im Logtext true.
% ggf. wird hier an den ggT-Prozess die Korrektur gesendet.
briefterm(AbsenderPid, GGTProName, CMi, CZeit, MinMi, Korrigieren) ->
    logge_ggtpro_status(GGTProName, CMi, CZeit, true),
    case CMi < MinMi of
        true -> 
            logge_status(io_lib:format("Neues Globales Mi: ~p (Alt: ~p)", [CMi, MinMi])), 
            NewMinMi = CMi;
        false ->
            NewMinMi = MinMi,
            case (Korrigieren and (CMi > MinMi)) of %Abfrage noetig, da CMi = MinMi sein koennte.
                true -> 
                    logge_status(io_lib:format("Korrektur an Client ~p gesendet", [GGTProName])),
                    AbsenderPid ! {sendy, MinMi};
                false -> 
                    donothing
            end
    end,
    NewMinMi.

% Fordert die Mis aller ggT-Prozesse an und gibt sie dann im Log aus.
prompt(GGTProNameList, NsPid) ->
    send_and_receive_mi(GGTProNameList, NsPid).

% Sendet an alle ggT-Prozesse ein 'tellmi', und erwartet ein 'mi' zurück.
% Arbeitet Sequentiell alle ggT-Prozesse ab!
send_and_receive_mi([], _NsPid) -> done;
send_and_receive_mi([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> continue;
        false -> 
            throw(ggtpronameUnkownForNs)
    end,
    HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
    HeadGGTProPid ! {self(), tellmi},
    receive
        {mi, Mi} -> logge_ggtpro_status(HeadGGTProName, Mi) 
    end,
    send_and_receive_mi(RestGGTProNames, NsPid).

% Fordert von allen ggT-Prozessen ein Lebenszeichen und loggt dies dann.
% Arbeitet Sequentiell!
nudge([], _NsPid) -> ok;
nudge([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> continue;
        false -> 
            throw(ggtpronameUnkownForNs)
    end,
    HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
    HeadGGTProPid ! {self(),pingGGT},
    receive 
        {pongGGT, HeadGGTProName} -> 
            logge_status(lists:flatten(io_lib:format("~p pong bekommen", [HeadGGTProName])))
        after 2000 -> 
            logge_status(lists:flatten(io_lib:format("GGTProName ~p mit ~p meldet sich nicht!", [HeadGGTProName, HeadGGTProPid])))
    end,
    nudge(RestGGTProNames, NsPid).

% Invertiert das Korrigieren Flag.
toggle(Korrigieren) ->
    logge_status("Korrigieren flag invertiert"),
    not(Korrigieren).

% Fährt sich selbst und die ggT-Prozesse herunter, startet sich dann jedoch Neu.
reset(GGTProNameList, NsPid) ->
    finalize(GGTProNameList, NsPid, true).

% Fährt sich selbst und die ggT-Prozesse herunter.
kill(GGTProNameList, NsPid) ->
    finalize(GGTProNameList, NsPid, false).

% Hier wird der Koordinator beim Nameservice 'unbind'ed und 'unregister'ed.
% Zu dem werden alle ggT-Prozesse herunter gefahren.
% Ist das Flag (3. Parameter) gesetzt so wird neu gestartet.
finalize(GGTProNameList, NsPid, Restart) ->
    NsPid ! {self(), {unbind, ?KONAME}},
    receive
        ok -> continue
    end,
    case whereis(?KONAME) of %TODO: problem, undefined in normal run.
        undefined -> logge_status("finalize findet ?KONAME nicht, wenn Test -> ok");
                     %Only for Test purposes! Because since its in the same process the name is always registered during normal run until unregistered here.
        _Any -> unregister(?KONAME)
    end,
    kill_all_ggtprocesses(GGTProNameList, NsPid),
    logge_status(lists:flatten(io_lib:format("Alle GGT-Prozesse herunter gefahren, selbst unregistered, Neustart = ~p", [Restart]))),
    case Restart of
        true -> start(NsPid);
        false -> killed
    end.

% Fährt alle ggT-Prozesse herunter.
kill_all_ggtprocesses([], _NsPid) -> done;
kill_all_ggtprocesses([HeadGGTProName | RestGGTProNames], NsPid) ->
    case ggtpropid_exists(HeadGGTProName, NsPid) of
        true -> 
            HeadGGTProPid = get_ggtpropid(HeadGGTProName, NsPid),
            HeadGGTProPid ! kill,
            kill_all_ggtprocesses(RestGGTProNames, NsPid);
        false -> 
            kill_all_ggtprocesses(RestGGTProNames, NsPid)
    end.

%------------
% Hilfsmethode um zu checken ob ein ggT-Prozessname im Nameservice bekannt ist.
ggtpropid_exists(GGTProName, NsPid) ->
    NsPid ! {self(), {lookup, GGTProName}},
    receive
        {pin, _GGTProPid} -> true;
        not_found -> false
    end.

% Hilfsmethode um einen ggT-Prozessnamen zu einer PID aufzulösen.
get_ggtpropid(GGTProName, NsPid) ->
    NsPid ! {self(), {lookup, GGTProName}},
    receive
        {pin, GGTProPid} -> 
            GGTProPid;
        not_found -> 
            throw(ggtpronameUnkownForNs)
    end.

% Holt einen Wert (key) aus der ganz oben spezifizierten Config datei.
hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult(?CONFIG_DATEI_NAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

% Hilfsmethode zum loggen des aktuellen CMis eines ggT-Prozesses
logge_ggtpro_status(GGTProName, CMi) ->
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi)", 
                            [GGTProName, CMi])
                    ),
    logge_status(LogNachricht),
    LogNachricht.

% Hilfsmethode zum loggen des aktuellen CMis, Zeit und TerminatedFlag eines ggT-Prozesses
logge_ggtpro_status(GGTProName, CMi, CZeit, TermFlag) ->
    LogNachricht = lists:flatten(
                        io_lib:format(
                            "~p meldet ~p(CMi) ~p(TermFlag) ~p(CZeit)", 
                            [GGTProName, CMi, TermFlag, CZeit])
                    ),
    logge_status(LogNachricht),
    LogNachricht.

% Hilfsmethode um einen bestimmten Text in den Log und SdtOut zu schreiben.
logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).