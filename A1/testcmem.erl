
-module(testcmem).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_TABELLENNAME, 'cmem_table').
-define(LOG_DATEI, "testcmem.log").
-define(ERINNERUNGS_ZEIT_SEK, 1).
-define(DEFAULT_NNR, 0).

initCMEM_1_test() ->
    {?ERINNERUNGS_ZEIT_SEK, []} = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI).

updateClient_1_test() ->
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    SelfPid = self(),
    NeueCMEM = cmem:updateClient(CMEM, SelfPid, 2, ?LOG_DATEI),
    {?ERINNERUNGS_ZEIT_SEK, [Tupel]} = NeueCMEM,
    {SelfPid, 2, _OldTS} = Tupel.

updateClient_2_test() ->
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    SelfPid = self(),
    NeueCMEM1 = cmem:updateClient(CMEM, a, 1, ?LOG_DATEI),
    NeueCMEM2 = cmem:updateClient(NeueCMEM1, b, 3, ?LOG_DATEI),
    NeueCMEM3 = cmem:updateClient(NeueCMEM2, c, 4, ?LOG_DATEI),
    NeueCMEM4 = cmem:updateClient(NeueCMEM3, SelfPid, 5, ?LOG_DATEI),
    {?ERINNERUNGS_ZEIT_SEK, Nachrichten4} = NeueCMEM4,
    {SelfPid, 5, _TS} = lists:keyfind(SelfPid, 1, Nachrichten4),
    NeueCMEM5 = cmem:updateClient(NeueCMEM4, SelfPid, 6, ?LOG_DATEI),
    {?ERINNERUNGS_ZEIT_SEK, Nachrichten5} = NeueCMEM5,
    {SelfPid, 6, _TS} = lists:keyfind(SelfPid, 1, Nachrichten5).

getClientNNr_1_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    NeueCMEM = cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    3 = cmem:getClientNNr(NeueCMEM, self()).

getClientNNr_2_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    Result = ?DEFAULT_NNR + 1,
    Result = cmem:getClientNNr(CMEM, self()).

getClientNNr_3_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    NeueCMEM = cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    Result = ?DEFAULT_NNR + 1,
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    Result = cmem:getClientNNr(NeueCMEM, self()).

pruefe_ts_und_gib_nnr_zurueck_1_test() ->
    NNr = 2,
    AltTS = erlang:timestamp(),
    2 = cmem:pruefe_ts_und_gib_nnr_zurueck(AltTS, ?ERINNERUNGS_ZEIT_SEK, NNr).

pruefe_ts_und_gib_nnr_zurueck_2_test() ->
    NNr = 2,
    AltTS = erlang:timestamp(),
    Result = ?DEFAULT_NNR,
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    Result = cmem:pruefe_ts_und_gib_nnr_zurueck(AltTS, ?ERINNERUNGS_ZEIT_SEK, NNr).

ts_ist_abglaufen_1_test() ->
    AltTS = erlang:timestamp(),
    ?assertNot(cmem:ts_ist_abglaufen(AltTS, ?ERINNERUNGS_ZEIT_SEK)).

ts_ist_abglaufen_2_test() ->
    AltTS = erlang:timestamp(),
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    ?assert(cmem:ts_ist_abglaufen(AltTS, ?ERINNERUNGS_ZEIT_SEK)).