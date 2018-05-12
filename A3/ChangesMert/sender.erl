-module(sender).

-export([
    start/1
]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start(LogFile) ->
    Pid = spawn(fun() -> loop({?NSNAME, ?NSNODE}, LogFile) end),
    logge_status("startet", LogFile),
    Pid.

% --------------------------------------------------

loop(Nameservice, LogFile) ->
    receive
        {send, Message} -> 
            send(Nameservice, Message)
            %spawn(fun() -> send_log(Message, LogFile) end)
    end,
    loop(Nameservice, LogFile).

send(Nameservice, Message) ->
	timer:sleep(20),
    Nameservice ! {multicast, Message}.

send_log(Message, LogFile) ->
    [ConvertedMessage] = messagehelper:convert_received_messages_from_byte([Message], [empty]),
    logge_status("Send ~p", [ConvertedMessage], LogFile).

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p - Send ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).
    
hole_wert_aus_config_mit_key(Key) ->
        {ok, ConfigListe} = file:consult('nameservice.cfg'),
        {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
        Value.

    