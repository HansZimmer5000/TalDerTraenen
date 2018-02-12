
-module(testclient).

-include_lib("eunit/include/eunit.hrl").

%        frage_nach_neuer_nnr/0,
%        erstelle_nachricht/2,
%        erstelle_nachrichten_text/1,
%        pruefe_nnr_und_sende_nachricht/2,
%        kalkuliere_neuen_intervall_sek/1,

%       frage_nach_neuer_nachricht/0,
%       empfangene_nachricht_ist_von_meinem_redakteur/2,
%TODO       logge_empfangene_nachricht/2,

%        zufalls_boolean/0,
%        element_ist_in_liste/2,
%        nachricht_zu_text/1,
%        neue_nnr_einfuegen/2

frage_nach_neuer_nnr_1_test() ->
    TestNNR = 1,
    ServerPid = spawn(fun() -> 
                            receive
                                    {AbsenderPID, getmsgid} -> AbsenderPID ! {nid, TestNNR}
                            end
                        end),
    TestNNR = client:frage_nach_neuer_nnr(ServerPid).

erstelle_nachricht_1_test() ->
    NNR = 1,
    TS = vsutil:now2string(erlang:timestamp()),
    Text =  io_lib:format("hostname1, gruppe1, team1, ~s", [TS]),
    TestNachricht = [NNR, Text, TS],
    ResultNachricht = client:erstelle_nachricht(NNR, TS),
    io:fwrite("\n"),
    io:fwrite(TestNachricht),
    io:fwrite("\n"),
    io:fwrite(ResultNachricht),
    TestNachricht == ResultNachricht.

erstelle_nachrichten_text_1_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Text = io_lib:format("hostname1, gruppe1, team1, ~s", [TS]),
    Result = client:erstelle_nachrichten_text(TS),
    io:fwrite("\n"),
    io:fwrite(Result),
    io:fwrite("\n"),
    io:fwrite(Text),
    Text == Result.



pruefe_nnr_und_sende_nachricht_1_test() ->
    ThisPid = self(),
    Nachricht = [1, "Text", vsutil:now2string(erlang:timestamp())],
    NNRListe = [1,4,5],
    ServerPid = spawn(fun() -> 
                            receive
                                    {dropmessage, _EmpfangeneNachricht} -> ThisPid ! ok
                            end
                        end),
    client:pruefe_nnr_und_sende_nachricht(ServerPid, Nachricht, NNRListe),
    receive
        ok -> ok
    end.

pruefe_nnr_und_sende_nachricht_2_test() ->
    ThisPid = self(),
    Nachricht = [1, "Text", vsutil:now2string(erlang:timestamp())],
    NNRListe = [1,4,2,3,5],
    ServerPid = spawn(fun() -> 
                            receive
                                    _Any -> ThisPid ! nok
                                    after 0 -> ThisPid ! ok
                            end
                        end),
    client:pruefe_nnr_und_sende_nachricht(ServerPid, Nachricht, NNRListe),
    receive
        ok -> ok
    end.


kalkuliere_neuen_intervall_sek_1_test() ->
    Result = client:kalkuliere_neuen_intervall_sek(2),
    case Result of
        3 -> true;
        2 -> true;
        _Else -> false
    end.

kalkuliere_neuen_intervall_sek_2_test() -> 
    2 = client:kalkuliere_neuen_intervall_sek("a").

kalkuliere_neuen_intervall_sek_3_test() ->
    2 = client:kalkuliere_neuen_intervall_sek(a).




frage_nach_neuer_nachricht_1_test() ->
    Nachricht = [3, "Text", vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp())],
    TerminatedFlag = false,
    ServerPid = spawn(fun() -> 
                        receive
                                {AbsenderPID, getmessages} -> AbsenderPID ! {reply, Nachricht, TerminatedFlag}
                        end
                    end),
    Nachricht = client:frage_nach_neuer_nachricht(ServerPid).

frage_nach_neuer_nachricht_2_test() ->
    Nachricht = [3, "Text", vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp()), vsutil:now2string(erlang:timestamp())],
    Ergebnis = [],
    TerminatedFlag = true,
    ServerPid = spawn(fun() -> 
                        receive
                                {AbsenderPID, getmessages} -> AbsenderPID ! {reply, Nachricht, TerminatedFlag}
                        end
                    end),
    Ergebnis = client:frage_nach_neuer_nachricht(ServerPid).


empfangene_nachricht_ist_von_meinem_redakteur_1_test() ->
    Nachricht = [1, "test", erlang:timestamp()],
    NNRListe = [3,4,5,1],
    true = client:empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NNRListe).

empfangene_nachricht_ist_von_meinem_redakteur_2_test() ->
    Nachricht = [1, "test", erlang:timestamp()],
    NNRListe = [3,4,5],
    false = client:empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NNRListe).

empfangene_nachricht_ist_von_meinem_redakteur_3_test() ->
    Nachricht = [1, "test", erlang:timestamp()],
    NNRListe = [],
    false = client:empfangene_nachricht_ist_von_meinem_redakteur(Nachricht, NNRListe).

logge_empfangene_nachricht_1_test() ->
    io:fwrite("Möglichkeit output anzuschauen? Da kein Logeintrag in der Datei, Eigentliche Arbeit in erstelle_empfangene_nachricht_logtext"),
    true = false.

erstelle_empfangene_nachricht_logtext_1_test() ->
    JetztTS = erlang:timestamp(),
    {JetztMegaSec, JetztSec, JetztMicroSec} = JetztTS,
    TS = {JetztMegaSec, JetztSec + 50, JetztMicroSec},

    Nachricht = [2, "Testtext", TS, TS, TS, TS],
    NummernListe = [1,2],
    LogText = client:erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    iolist_to_binary("Empfangene Nachricht 'Testtext' ist aus der Zukunft ist von meinem Redakteur") =:= iolist_to_binary(LogText).

erstelle_empfangene_nachricht_logtext_2_test() ->
    JetztTS = erlang:timestamp(),
    {JetztMegaSec, JetztSec, JetztMicroSec} = JetztTS,
    TS = {JetztMegaSec, JetztSec - 50, JetztMicroSec},

    Nachricht = [2, "Testtext", TS, TS, TS, TS],
    NummernListe = [1,2],
    LogText = client:erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    iolist_to_binary("Empfangene Nachricht 'Testtext'  ist von meinem Redakteur") =:= iolist_to_binary(LogText).

erstelle_empfangene_nachricht_logtext_3_test() ->
    JetztTS = erlang:timestamp(),
    {JetztMegaSec, JetztSec, JetztMicroSec} = JetztTS,
    TS = {JetztMegaSec, JetztSec - 50, JetztMicroSec},

    Nachricht = [2, "Testtext", TS, TS, TS, TS],
    NummernListe = [1],
    LogText = client:erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    iolist_to_binary("Empfangene Nachricht 'Testtext'  ") =:= iolist_to_binary(LogText).

erstelle_empfangene_nachricht_logtext_4_test() ->
    JetztTS = erlang:timestamp(),
    {JetztMegaSec, JetztSec, JetztMicroSec} = JetztTS,
    TS = {JetztMegaSec, JetztSec + 50, JetztMicroSec},

    Nachricht = [2, "Testtext", TS, TS, TS, TS],
    NummernListe = [1],
    LogText = client:erstelle_empfangene_nachricht_logtext(Nachricht, NummernListe),
    iolist_to_binary("Empfangene Nachricht 'Testtext' ist aus der Zukunft ") =:= iolist_to_binary(LogText).


zufalls_boolean_1_test() ->
    case client:zufalls_boolean() of
        true -> true;
        false -> true;
        _Else -> false
    end.

element_ist_in_liste_1_test() ->
    client:element_ist_in_liste(a, [a,b]).

element_ist_in_liste_2_test() ->
    false = client:element_ist_in_liste(c, [a,b]).

element_ist_in_liste_3_test() ->
    false = client:element_ist_in_liste([],[a,b]).


neue_nnr_einfuegen_1_test() ->
    [1] = client:neue_nnr_einfuegen(1,[]).

neue_nnr_einfuegen_2_test() ->
    [1,2,3] = client:neue_nnr_einfuegen(1,[2,3]).

nachricht_zu_text_1_test() ->
    TS = {1516, 47115, 874000},
    Nachricht = [1, 
                "Test", 
                vsutil:now2string(TS),
                vsutil:now2string(TS),
                vsutil:now2string(TS),
                vsutil:now2string(TS)
                ],
    "1, Test, 15.01 21:11:55,874|, 15.01 21:11:55,874|, 15.01 21:11:55,874|, 15.01 21:11:55,874|" = client:nachricht_zu_text(Nachricht).

nachricht_zu_text_2_test() ->
    TS = {1516, 47115, 874000},
    Nachricht = [1, 
                "Test", 
                vsutil:now2string(TS)
                ],
    "1, Test, 15.01 21:11:55,874|" = client:nachricht_zu_text(Nachricht).