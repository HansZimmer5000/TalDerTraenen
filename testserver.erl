
-module(testserver).

-include_lib("eunit/include/eunit.hrl").


getmessages_abfertigen_1_test() ->
    %TODO Gib Server Nachricht 1, und frage dann nach Nachricht, ergebnis sollte Nachricht 1 sein! Erst richtig machbar wenn HBQ/DLQ voll implementiert.
    ServerPid = starte_server(),
    fahre_server_vorzeitig_herunter(ServerPid),
    io:fwrite("Warten auf HBQ/DLQ Implementation"),
    true = false.

dropmessage_abfertigen_1_test() ->
    %TODO Wie genau testen? Irgendwie an HbQ rankommen -> da rein schauen?
    ServerPid = starte_server(),
    fahre_server_vorzeitig_herunter(ServerPid),
    io:fwrite("Warten auf HBQ/DLQ Implementation"),
    true = false.

getmsgid_abfertigen_1_test() ->
    AktuelleNNR = server:getmsgid_abfertigen(self(), 0),
    receive 
        {nid, 1} -> Temp_Ergebnis = true;
        _Any -> Temp_Ergebnis = false
    end,
    AktuelleNNR = 1,
    Temp_Ergebnis.

getmsgid_abfertigen_2_test() ->
    ServerPid = starte_server(),
    ServerPid ! {self(), getmsgid},
    receive
        {nid, 1} -> fahre_server_vorzeitig_herunter(ServerPid)
        after 3 ->  io:fwrite("Nichts bekommen."),
                    fahre_server_vorzeitig_herunter(ServerPid)
    end.




starte_server() ->
    ServerPid = server:start(),
    ServerPid.

fahre_server_vorzeitig_herunter(ServerPid) ->
    exit(ServerPid, 'test vorbei').