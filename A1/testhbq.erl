
-module(testhbq).

-include_lib("eunit/include/eunit.hrl").

-define(DLQSIZE, 5).
-define(MAX_DELAY, 1000).

start_1_test() ->
    HBQPid = hbq:start(),
    HBQPid ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> ?assert(true)
        after ?MAX_DELAY -> ?assert(false)
    end,
    exit(HBQPid, kill).

wait_for_init_1_test() ->
    HBQPid = spawn(fun() -> hbq:wait_for_init() end),
    HBQPid ! {self(), {request, initHBQ}},
    receive
        {reply, ok} -> ?assert(true)
        after ?MAX_DELAY -> ?assert(false)
    end,
    exit(HBQPid, kill).

init_hbq_1_test() ->
    hbq:init_hbq(self()),
    receive
        {reply, ok} -> ?assert(true)
        after ?MAX_DELAY -> ?assert(false)
    end.

push_hbq_1_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht2 = [2, "Text", TS],
    HBQ = [],
    DLQ = {?DLQSIZE, [Nachricht1]},
    ServerPid = self(),
    ThisPid = self(),
    _HBQPid = spawn(fun() -> 
                        Result = hbq:push_hbq(ServerPid, Nachricht2, HBQ, DLQ),
                        ThisPid ! Result 
                    end),
    receive
        Any -> ?assertEqual({reply, ok}, Any)
        after ?MAX_DELAY -> ?assert(false)
    end,
    receive
        Result -> 
            {NeueHBQ, NeueDLQ} = Result,
            {?DLQSIZE, NeueDLQNachrichten} = NeueDLQ,
            [DLQNachricht1, DLQNachricht2] = NeueDLQNachrichten,
            
            [2, "Text", TS, _HBQInTS, _DLQInTS] = DLQNachricht1,
            ?assertEqual(Nachricht1, DLQNachricht2),
            ?assertEqual(HBQ, NeueHBQ)
        after ?MAX_DELAY -> ?assert(false)
    end.
    

deliver_nachricht_1_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [1, "Text", TS, TS, TS],
    DLQ = {?DLQSIZE, [Nachricht]},
    ClientPid = self(),
    ServerPid = self(),
    _HBQPid = spawn(fun() -> hbq:deliver_nachricht(ServerPid, 1, ClientPid, DLQ) end),
    receive
        {reply, SentMsgNum} -> ?assertEqual(1, SentMsgNum)
        after ?MAX_DELAY -> ?assert(false)
    end,
    receive
        {reply, ZuSendendeNachricht, TermiatedFlag} -> 
            [1, "Text", TS, TS, TS, _DLQOutTS] = ZuSendendeNachricht,
            ?assert(TermiatedFlag)
        after ?MAX_DELAY -> ?assert(false)
    end.

deliver_nachricht_2_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [2, "Text", TS, TS, TS],
    DLQ = {?DLQSIZE, [Nachricht]},
    ClientPid = self(),
    ServerPid = self(),
    _HBQPid = spawn(fun() -> hbq:deliver_nachricht(ServerPid, 1, ClientPid, DLQ) end),
    receive
        Any2 -> 
            {reply, EmpfangeneNachricht, TerminatedFlag} = Any2,
            [2, "Text", TS, TS, TS, _] = EmpfangeneNachricht,
            ?assert(TerminatedFlag)
        after ?MAX_DELAY -> ?assert(false)
    end,
    receive
        Any1 -> 
            {reply, SentMsgNum} = Any1,
            ?assertEqual(2, SentMsgNum)
        after ?MAX_DELAY -> ?assert(false)
    end.

deliver_nachricht_3_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [2, "Text", TS, TS, TS],
    DLQ = {?DLQSIZE, [Nachricht]},
    ClientPid = self(),
    ServerPid = self(),
    _HBQPid = spawn(fun() -> hbq:deliver_nachricht(ServerPid, 3, ClientPid, DLQ) end),
    receive
        Any2 -> 
            {reply, EmpfangeneNachricht, TerminatedFlag} = Any2,
            [0, "Angeforderte Nachricht nicht vorhanden." | _] = EmpfangeneNachricht,
            ?assert(TerminatedFlag)
        after ?MAX_DELAY -> ?assert(false)
    end,
    receive
        Any1 -> 
            {reply, SentMsgNum} = Any1,
            ?assertEqual(0, SentMsgNum)
        after ?MAX_DELAY -> ?assert(false)
    end.

deliver_nachricht_4_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht1 = [2, "Text", TS, TS, TS],
    Nachricht2 = [4, "Fülle Lücke von 3 bis 4", TS, TS, TS],
    Nachricht3 = [5, "Text", TS, TS, TS],
    DLQ = {?DLQSIZE, [Nachricht3, Nachricht2, Nachricht1]},
    ClientPid = self(),
    ServerPid = self(),
    _HBQPid = spawn(fun() -> hbq:deliver_nachricht(ServerPid, 3, ClientPid, DLQ) end),
    receive
        Any2 -> 
            {reply, EmpfangeneNachricht, TerminatedFlag} = Any2,
            [4, "Fülle Lücke von 3 bis 4", TS, TS, TS, _] = EmpfangeneNachricht,
            ?assertNot(TerminatedFlag)
        after ?MAX_DELAY -> ?assert(false)
    end,
    receive
        Any1 -> 
            {reply, SentMsgNum} = Any1,
            ?assertEqual(4, SentMsgNum)
        after ?MAX_DELAY -> ?assert(false)
    end.

delete_hbq_1_test() ->
    DLQ = {?DLQSIZE, []},
    ThisPid = self(),
    HBQPid = spawn(fun() -> hbq:delete_hbq(ThisPid, DLQ) end),
    register(wk, HBQPid),
    receive
        Any -> ?assertEqual({reply, ok}, Any)
        after ?MAX_DELAY -> ?assert(false)
    end. 

wird_erwartet_1_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    DLQ = {?DLQSIZE, [[1, "Text", TS, TS ,TS]]},
    Nachricht = [2, "Text2", TS, TS],
    ?assert(hbq:wird_erwartet(Nachricht, DLQ)).

pruefe_naechste_nachricht_und_pushe_1_test() ->
    HBQ = [],
    DLQ = {?DLQSIZE, []},
    ?assertEqual({[], DLQ}, hbq:pruefe_naechste_nachricht_und_pushe(HBQ, DLQ)).

pruefe_naechste_nachricht_und_pushe_2_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [1, "Text", TS, TS],
    HBQ = [Nachricht],
    DLQ = {?DLQSIZE, []},
    {NeueHBQ, NeueDLQ} = hbq:pruefe_naechste_nachricht_und_pushe(HBQ, DLQ),

    {?DLQSIZE, NeueDLQNachrichten} = NeueDLQ,
    [ErsteDLQNachricht | _Rest] = NeueDLQNachrichten,
    [1, "Text", TS, TS, _DLQINTS] = ErsteDLQNachricht,
    ?assertEqual([], NeueHBQ).

pruefe_naechste_nachricht_und_pushe_3_test() ->
    TS = vsutil:now2string(erlang:timestamp()),
    Nachricht = [2, "Text", TS, TS],
    HBQ = [Nachricht],
    DLQ = {?DLQSIZE, []},
    ?assertEqual(
        {[Nachricht], {?DLQSIZE, []}}, 
        hbq:pruefe_naechste_nachricht_und_pushe(HBQ, DLQ)).

in_hbq_einfuegen_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    HBQ = [Nachricht1, Nachricht4],
    ?assertEqual(
        [Nachricht1, Nachricht4, Nachricht5],
        hbq:in_hbq_einfuegen(Nachricht5, HBQ)
    ).

in_hbq_einfuegen_2_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    HBQ = [],
    ?assertEqual(
        [Nachricht1],
        hbq:in_hbq_einfuegen(Nachricht1, HBQ)
    ).

in_hbq_einfuegen_3_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    HBQ = [Nachricht1, Nachricht5],
    ?assertEqual(
        [Nachricht1, Nachricht4, Nachricht5],
        hbq:in_hbq_einfuegen(Nachricht4, HBQ)
    ).

pruefe_limit_und_fuelle_spalte_1_test() ->    
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    Nachricht6 = [6, "Text", TS, TS],
    Nachricht7 = [7, "Text", TS, TS],
    HBQ = [Nachricht4, Nachricht5, Nachricht6, Nachricht7],
    DLQ = {?DLQSIZE, [Nachricht1]},
    {?DLQSIZE, [DLQNachricht1, DLQNachricht2]} = hbq:pruefe_limit_und_fuelle_spalte(HBQ, DLQ, ?DLQSIZE),
    [3, "Error Nachricht zum Luecke von 2 bis 3 zu fuellen", TS, TS, _TS] = DLQNachricht1,
    [1, "Text", TS, TS, TS] = DLQNachricht2.

pruefe_limit_und_fuelle_spalte_2_test() ->    
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    Nachricht5 = [5, "Text", TS, TS],
    Nachricht6 = [6, "Text", TS, TS],
    HBQ = [Nachricht4, Nachricht5, Nachricht6],
    DLQ = {?DLQSIZE, [Nachricht1]},
    {?DLQSIZE, [DLQNachricht1]} = hbq:pruefe_limit_und_fuelle_spalte(HBQ, DLQ, ?DLQSIZE),
    [1, "Text", TS, TS, TS] = DLQNachricht1.

finde_und_fuelle_spalte_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    HBQ = [Nachricht4],
    DLQ = {?DLQSIZE, [Nachricht1]},
    {?DLQSIZE, [DLQNachricht1, DLQNachricht2]} = hbq:finde_und_fuelle_spalte(HBQ, DLQ),
    [3, "Error Nachricht zum Luecke von 2 bis 3 zu fuellen", TS, TS, _TS] = DLQNachricht1,
    [1, "Text", TS, TS, TS] = DLQNachricht2.

finde_spalte_1_test() ->
    TS = erlang:timestamp(),
    Nachricht1 = [1, "Text", TS, TS, TS],
    Nachricht4 = [4, "Text", TS, TS],
    HBQ = [Nachricht4],
    DLQ = {?DLQSIZE, [Nachricht1]},
    ?assertEqual(
        {2, 3},
        hbq:finde_spalte(HBQ, DLQ)
    ).

erstelle_spalt_nachricht_1_test() ->
    SpaltStartNNr = 2,
    SpaltEndeNNr = 3,
    [SpaltNNr, SpaltText, _TS, _TS] = hbq:erstelle_spalt_nachricht(SpaltStartNNr, SpaltEndeNNr),
    ?assertEqual(SpaltEndeNNr, SpaltNNr),
    ?assertEqual("Error Nachricht zum Luecke von 2 bis 3 zu fuellen", SpaltText).


