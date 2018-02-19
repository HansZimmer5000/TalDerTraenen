
-module(testdlq).

-include_lib("eunit/include/eunit.hrl").

-define(LOG_DATEI, 'testdlq.log').
-define(SIZE, 10).
-define(KLEINERE_SIZE, 2).

initDLQ_1_test() ->
    [?SIZE, []] = dlq:initDLQ(?SIZE, ?LOG_DATEI).

expectedNr_1_test() ->
    TS = erlang:timestamp(),
    Nachricht = [1, "Text", TS, TS, TS],
    2 = dlq:expectedNr([?SIZE, [Nachricht]]).

expectedNr_2_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [1, "Text", TS, TS, TS],
    3 = dlq:expectedNr([?SIZE, [Nachricht1, Nachricht2]]).

holeMaxNNr_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [1, "Text", TS, TS, TS],
    2 = dlq:holeMaxNNr([Nachricht1, Nachricht2]).

holeMaxNNr_2_test() ->
    0 = dlq:holeMaxNNr([]).


push2DLQ_1_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    NachrichtOriginal = [1, "Text", TS, TS],
    NeueDLQ = dlq:push2DLQ(NachrichtOriginal, DLQ, ?LOG_DATEI),
    [?SIZE, [NachrichtMitTS]] = NeueDLQ,
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
    [?KLEINERE_SIZE, [[3, "Text", TS, TS, _TS], [2, "Text", TS, TS, _TS]]] = NeueDLQ3.

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


entferneLetztesListenElement_1_test() ->
    [] = dlq:entferneLetztesListenElement([1]).

entferneLetztesListenElement_2_test() ->
    [] = dlq:entferneLetztesListenElement([]).

entferneLetztesListenElement_3_test() ->
    [1,3,4] = dlq:entferneLetztesListenElement([1,3,4,5]).


dLQIstVoll_1_test() ->
    DLQ = dlq:initDLQ(0, ?LOG_DATEI),
    true = dlq:dLQIstVoll(DLQ).

dLQIstVoll_2_test() ->
    DLQ = dlq:initDLQ(1, ?LOG_DATEI),
    false = dlq:dLQIstVoll(DLQ).

dLQIstVoll_3_test() ->
    DLQ = dlq:initDLQ(1, ?LOG_DATEI),
    TS = erlang:timestamp(),
    NeueDLQ = dlq:push2DLQ([1, "Text", TS, TS], DLQ, ?LOG_DATEI),
    true = dlq:dLQIstVoll(NeueDLQ).


holeNachricht_1_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    TS = erlang:timestamp(),
    [?SIZE, Nachrichten] = dlq:push2DLQ([1, "Text", TS, TS], DLQ, ?LOG_DATEI),
    [1, "Text", TS, TS, _TS] = dlq:holeNachricht(Nachrichten, 1).

holeNachricht_2_test() ->
    DLQ = dlq:initDLQ(?SIZE, ?LOG_DATEI),
    [] = dlq:holeNachricht(DLQ, 1).


erstelleErrNachrichtFurFehlendeNNr_1_test() ->
    [0, "Angeforderte Nachricht nicht vorhanden." | _Timestamps] = dlq:erstelleErrNachrichtFurFehlendeNNr().