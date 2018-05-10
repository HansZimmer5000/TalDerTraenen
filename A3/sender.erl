-module(sender).

-export([
    start/1
]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start(LogFile) ->
    Pid = spawn(fun() -> loop({?NSNAME, ?NSNODE}) end),
    logge_status("startet", LogFile),
    Pid.

% --------------------------------------------------

loop(Nameservice) ->
    receive
        {send, Message} -> send(Nameservice, Message)
    end,
    loop(Nameservice).

send(Nameservice, Message) ->
    Nameservice ! {multicast, Message}.

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

    