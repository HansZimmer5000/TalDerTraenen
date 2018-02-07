
-module(testserver).

-include_lib("eunit/include/eunit.hrl").

getmsgid_abfertigen_1_test() ->
    AktuelleNNR = server:getmsgid_abfertigen(self(), 0),
    receive 
        {nid, 1} -> Temp_Ergebnis = true;
        _Any -> Temp_Ergebnis = false
    end,
    AktuelleNNR = 1,
    Temp_Ergebnis.