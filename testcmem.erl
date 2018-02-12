
-module(testcmem).

-include_lib("eunit/include/eunit.hrl").

-define(ETS_TABELLENNAME, 'cmem_table').
-define(LOG_DATEI, "testcmem.log").

initCMEM_1_test() ->
    cmem:initCMEM(2, ?LOG_DATEI),
    ets:delete(?ETS_TABELLENNAME).

updateClient_1_test() ->
    CMEM = cmem:initCMEM(2, ?LOG_DATEI),
    cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    [{_, 2, _}] = ets:lookup(?ETS_TABELLENNAME, self()),
    ets:delete(?ETS_TABELLENNAME).

getClientNNr_1_test() ->    
    CMEM = cmem:initCMEM(2, ?LOG_DATEI),
    cmem:updateClient(CMEM, self(), 2, ?LOG_DATEI),
    2 = cmem:getClientNNr(CMEM, self()),
    ets:delete(?ETS_TABELLENNAME).

getClientNNr_2_test() ->    
    CMEM = cmem:initCMEM(2, ?LOG_DATEI),
    1 = cmem:getClientNNr(CMEM, self()),
    ets:delete(?ETS_TABELLENNAME).