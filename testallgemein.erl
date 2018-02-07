-module(testallgemein).

-include_lib("eunit/include/eunit.hrl").

-define(CONFIG_FILENAME, "test.cfg").


hohle_wert_aus_config_mit_key_1_test() ->
    "testname" = hohle_wert_aus_config_mit_key(testname).


hohle_wert_aus_config_mit_key(Key) ->
    %log_status(extractValueFromConfig,io_lib:format("Key: ~p",[Key])),
    {ok, ConfigListe} = file:consult(?CONFIG_FILENAME),
    {ok, Value} = werkzeug:get_config_value(Key, ConfigListe),
    Value.