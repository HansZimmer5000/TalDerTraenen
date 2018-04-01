
-module(testdlq).

-include_lib("eunit/include/eunit.hrl").

-define(LOG_DATEI, 'testdlq.log').
-define(SIZE, 10).
-define(KLEINERE_SIZE, 2).

initDLQ_1_test() ->
    {?SIZE, []} = dlq:initDLQ(?SIZE, ?LOG_DATEI).

expectedNr_1_test() ->
    TS = erlang:timestamp(),
    Nachricht = [1, "Text", TS, TS, TS],
    2 = dlq:expectedNr({?SIZE, [Nachricht]}).

expectedNr_2_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [1, "Text", TS, TS, TS],
    3 = dlq:expectedNr({?SIZE, [Nachricht1, Nachricht2]}).

hole_max_nnr_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [1, "Text", TS, TS, TS],
    2 = dlq:hole_max_nnr([Nachricht1, Nachricht2]).

hole_max_nnr_2_test() ->
    0 = dlq:hole_max_nnr([]).


push2DLQ_1_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    NachrichtOriginal = [1, "Text", TS, TS],
    NeueDLQ = dlq:push2DLQ(NachrichtOriginal, DLQ, ?LOG_DATEI),
    {?SIZE, [NachrichtMitTS]} = NeueDLQ,
    [NNrOriginal, TextOriginal, TSOriginal, TSOriginal] = NachrichtOriginal,
    [NNrOriginal, TextOriginal, TSOriginal, TSOriginal, _TS] = NachrichtMitTS.

push2DLQ_2_test() ->
    DLQ = dlq:initDLQ(?KLEINERE_SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    Nachricht2 = [2, "Text", TS, TS],
    Nachricht3 = [3, "Text", TS, TS],
    NeueDLQ1 = dlq:push2DLQ(Nachricht1, DLQ, ?LOG_DATEI),
    NeueDLQ2 = dlq:push2DLQ(Nachricht2, NeueDLQ1, ?LOG_DATEI),
    NeueDLQ3 = dlq:push2DLQ(Nachricht3, NeueDLQ2, ?LOG_DATEI),
    io:fwrite("~w", [NeueDLQ3]),
    {?KLEINERE_SIZE, [[3, "Text", TS, TS, _TS], [2, "Text", TS, TS, _TS]]} = NeueDLQ3.

deliverMSG_1_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    Nachricht = [1, "Text", TS, TS],
    NeueDLQ = dlq:push2DLQ(Nachricht, DLQ, ?LOG_DATEI),
    1 = dlq:deliverMSG(1, self(), NeueDLQ, ?LOG_DATEI),
    receive
        {reply, EmpfangeneNachricht, TerminatedFlag} ->
            [1, "Text", TS, TS, _TS, _TS] = EmpfangeneNachricht,
            true = TerminatedFlag
    end.

deliverMSG_2_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    0 = dlq:deliverMSG(1, self(), DLQ, ?LOG_DATEI),
    receive
        {reply, EmpfangeneNachricht, TerminatedFlag} ->
            [0, _Text, _TS, _TS, _TS, _TS] = EmpfangeneNachricht,
            true = TerminatedFlag
    end.


entferne_letztes_listen_element_1_test() ->
    [] = dlq:entferne_letztes_listen_element([1]).

entferne_letztes_listen_element_2_test() ->
    [] = dlq:entferne_letztes_listen_element([]).

entferne_letztes_listen_element_3_test() ->
    [1,3,4] = dlq:entferne_letztes_listen_element([1,3,4,5]).


dlq_ist_voll_1_test() ->
    DLQ = dlq:initDLQ(0, ?LOG_DATEI),
    ?assert(dlq:dlq_ist_voll(DLQ)).

dlq_ist_voll_2_test() ->
    DLQ = dlq:initDLQ(1, ?LOG_DATEI),
    false = dlq:dlq_ist_voll(DLQ).

dlq_ist_voll_3_test() ->
    DLQ = dlq:initDLQ(1, ?LOG_DATEI),
    TS = erlang:timestamp(),
    NeueDLQ = dlq:push2DLQ([1, "Text", TS, TS], DLQ, ?LOG_DATEI),
    ?assert(dlq:dlq_ist_voll(NeueDLQ)).

hole_naechst_groessere_nnr_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [4, "Fülle Lücke von 3 bis 4", TS, TS, TS],
    Nachricht3 = [5, "Text", TS, TS, TS],
    ReverseDLQNachrichten = [Nachricht1, Nachricht2, Nachricht3],
    AusgangsNNr = 3,
    ?assertEqual(
        4,
        dlq:hole_naechst_groessere_nnr(ReverseDLQNachrichten, AusgangsNNr)
    ).

hole_naechst_groessere_nnr_2_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    ReverseDLQNachrichten = [Nachricht1],
    AusgangsNNr = 1,
    ?assertEqual(
        2,
        dlq:hole_naechst_groessere_nnr(ReverseDLQNachrichten, AusgangsNNr)
    ).

hole_nachricht_1_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    {?SIZE, Nachrichten} = dlq:push2DLQ([1, "Text", TS, TS], DLQ, ?LOG_DATEI),
    [1, "Text", TS, TS, _TS] = dlq:hole_nachricht(Nachrichten, 1).

hole_nachricht_2_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    ?assertEqual([], dlq:hole_nachricht(DLQ, 1)).


erstelleErrNachricht_1_test() ->
    [0, "Angeforderte Nachricht nicht vorhanden." | _Timestamps] = dlq:erstelleErrNachricht().