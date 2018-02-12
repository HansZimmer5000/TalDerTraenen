-module(testallgemein).

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_FILENAME, "test.cfg").
-define(LOG_DATEI_NAME, "test.log").


hole_wert_aus_config_mit_key_1_test() ->
    "testname" = hole_wert_aus_config_mit_key(testname).


% Allgemeine Funktionen in jedem .erl vorhanden.
hole_wert_aus_config_mit_key(Key) ->
    logge_status(io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).

logge_nachricht_status(Nachricht, Status) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht).