-module(testallgemein).

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_FILENAME, "test.cfg").
-define(LOG_DATEI_NAME, "test.log").


hohle_wert_aus_config_mit_key_1_test() ->
    "testname" = hohle_wert_aus_config_mit_key(testname).


% Allgemeine Funktionen in jedem .erl vorhanden.
hohle_wert_aus_config_mit_key(Key) ->
    logge_status(io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.", [AktuelleZeit, Inhalt]),
    LogNachrichtenFlatten = lists:flatten(LogNachricht), %TODO Braucht man das?
    io:fwrite(io_lib:format("~s\n" ,[LogNachrichtenFlatten])),
    util:logging(?LOG_DATEI_NAME, LogNachrichtenFlatten).

logge_nachricht_status(Nachricht, Status) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    LogNachrichtenFlatten = lists:flatten(LogNachricht),
    logge_status(LogNachrichtenFlatten).

