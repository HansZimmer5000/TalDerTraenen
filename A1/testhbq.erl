
-module(testhbq).

-include_lib("eunit/include/eunit.hrl").

-define(DLQSIZE, 5).

start_1_test() ->
    HBQPid = hbq:start(),
    HBQPid ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> ?assert(true)
        after 1000 -> ?assert(false)
    end,
    exit(HBQPid, kill).

waitForInit_1_test() ->
    HBQPid = spawn(fun() -> hbq:waitForInit() end),
    HBQPid ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> ?assert(true)
        after 1000 -> ?assert(false)
    end,
    exit(HBQPid, kill).

initHBQHandler_1_test() ->
    hbq:initHBQHandler(self()),
    receive
        {reply, ok} -> ?assert(true)
        after 1000 -> ?assert(false)
    end.

receive_loop_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

pushHBQHandler_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).
    %hbq:pushHBQHandler(PID, Nachricht, HoldbackQueue, DeliveryQueue)

deliverMSGHandler_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

deleteHBQHandler_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

isInOrder_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

pruefeNaechsteNachrichtUndPushe_1_test() ->
    io:fwrite("Not implemented yet"),
    ?assert(false).

inHBQeinfuegen_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    HBQ = [Nachricht1, Nachricht4],
    ?assertEqual(
        [Nachricht1, Nachricht4, Nachricht5],
        hbq:inHBQeinfuegen(Nachricht5, HBQ)
    ).

inHBQeinfuegen_2_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    HBQ = [],
    ?assertEqual(
        [Nachricht1],
        hbq:inHBQeinfuegen(Nachricht1, HBQ)
    ).

inHBQeinfuegen_3_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    HBQ = [Nachricht1, Nachricht5],
    ?assertEqual(
        [Nachricht1, Nachricht4, Nachricht5],
        hbq:inHBQeinfuegen(Nachricht4, HBQ)
    ).

pruefeLimitUndFuelleSpalte_1_test() ->    
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    Nachricht6 = [6, "Text", TS, TS],
    Nachricht7 = [7, "Text", TS, TS],
    HBQ = [Nachricht4, Nachricht5, Nachricht6, Nachricht7],
    DLQ = [?DLQSIZE, [Nachricht1]],
    [?DLQSIZE, [DLQNachricht1, DLQNachricht2]] = hbq:pruefeLimitUndFuelleSpalte(HBQ, DLQ, ?DLQSIZE),
    [3, "Error Nachricht zum Luecke von 2 bis 3 zu fuellen", TS, TS, _TS] = DLQNachricht1,
    [1, "Text", TS, TS, TS] = DLQNachricht2.

pruefeLimitUndFuelleSpalte_2_test() ->    
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    Nachricht6 = [6, "Text", TS, TS],
    HBQ = [Nachricht4, Nachricht5, Nachricht6],
    DLQ = [?DLQSIZE, [Nachricht1]],
    [?DLQSIZE, [DLQNachricht1]] = hbq:pruefeLimitUndFuelleSpalte(HBQ, DLQ, ?DLQSIZE),
    [1, "Text", TS, TS, TS] = DLQNachricht1.

sucheUndFuelleSpalte_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    HBQ = [Nachricht4],
    DLQ = [?DLQSIZE, [Nachricht1]],
    [?DLQSIZE, [DLQNachricht1, DLQNachricht2]] = hbq:sucheUndFuelleSpalte(HBQ, DLQ),
    [3, "Error Nachricht zum Luecke von 2 bis 3 zu fuellen", TS, TS, _TS] = DLQNachricht1,
    [1, "Text", TS, TS, TS] = DLQNachricht2.

sucheSpalte_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    HBQ = [Nachricht4],
    DLQ = [?DLQSIZE, [Nachricht1]],
    ?assertEqual(
        {2, 3},
        hbq:sucheSpalte(HBQ, DLQ)
    ).

erstelleSpaltNachricht_1_test() ->
    SpaltStartNNr = 2,
    SpaltEndeNNr = 3,
    [SpaltNNr, SpaltText, _TS, _TS] = hbq:erstelleSpaltNachricht(SpaltStartNNr, SpaltEndeNNr),
    ?assertEqual(SpaltEndeNNr, SpaltNNr),
    ?assertEqual("Error Nachricht zum Luecke von 2 bis 3 zu fuellen", SpaltText).


