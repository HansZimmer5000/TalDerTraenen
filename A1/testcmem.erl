
-module(testcmem).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_TABELLENNAME, 'cmem_table').
-define(LOG_DATEI, "testcmem.log").
-define(ERINNERUNGS_ZEIT_SEK, 1).
-define(DEFAULT_NNR, 0).

initCMEM_1_test() ->
    [?ERINNERUNGS_ZEIT_SEK, []] = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI).

updateClient_1_test() ->
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    SelfPid = self(),
    NeueCMEM = cmem:updateClient(CMEM, SelfPid, 2, ?LOG_DATEI),
    [?ERINNERUNGS_ZEIT_SEK, [Tupel]] = NeueCMEM,
    {SelfPid, 2, _OldTS} = Tupel.

updateClient_2_test() ->
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    SelfPid = self(),
    NeueCMEM1 = cmem:updateClient(CMEM, a, 1, ?LOG_DATEI),
    NeueCMEM2 = cmem:updateClient(NeueCMEM1, b, 3, ?LOG_DATEI),
    NeueCMEM3 = cmem:updateClient(NeueCMEM2, c, 4, ?LOG_DATEI),
    NeueCMEM4 = cmem:updateClient(NeueCMEM3, SelfPid, 5, ?LOG_DATEI),
    [?ERINNERUNGS_ZEIT_SEK, [Tupel1, Tupel2, Tupel3, Tupel4]] = NeueCMEM4,
    {SelfPid, 5, _OldTS} = Tupel4,
    NeueCMEM5 = cmem:updateClient(NeueCMEM4, SelfPid, 6, ?LOG_DATEI),
    [?ERINNERUNGS_ZEIT_SEK, [Tupel1, Tupel2, Tupel3, Tupel5]] = NeueCMEM5,
    {SelfPid, 6, _OldTS} = Tupel5.

getClientNNr_1_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    NeueCMEM = cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    2 = cmem:getClientNNr(NeueCMEM, self()).

getClientNNr_2_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    ?DEFAULT_NNR = cmem:getClientNNr(CMEM, self()).

getClientNNr_3_test() ->    
    CMEM = cmem:initCMEM(?ERINNERUNGS_ZEIT_SEK, ?LOG_DATEI),
    NeueCMEM = cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    ?DEFAULT_NNR = cmem:getClientNNr(NeueCMEM, self()).

pruefeTSUndGibNNrZuruck_1_test() ->
    NNr = 2,
    AltTS = erlang:timestamp(),
    NNr = cmem:pruefeTSUndGibNNrZuruck(AltTS, ?ERINNERUNGS_ZEIT_SEK, NNr).

pruefeTSUndGibNNrZuruck_2_test() ->
    NNr = 2,
    AltTS = erlang:timestamp(),
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    ?DEFAULT_NNR = cmem:pruefeTSUndGibNNrZuruck(AltTS, ?ERINNERUNGS_ZEIT_SEK, NNr).

tSIstAbglaufen_1_test() ->
    AltTS = erlang:timestamp(),
    false = cmem:tSIstAbglaufen(AltTS, ?ERINNERUNGS_ZEIT_SEK).

tSIstAbglaufen_2_test() ->
    AltTS = erlang:timestamp(),
    timer:sleep(timer:seconds(?ERINNERUNGS_ZEIT_SEK + 1)),
    true = cmem:tSIstAbglaufen(AltTS, ?ERINNERUNGS_ZEIT_SEK).