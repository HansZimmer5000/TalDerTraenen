-module(sender).

-export([
    start/2
]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

start(PayloadServerPid, LogFile) ->
    Pid = spawn(fun() -> loop({?NSNAME, ?NSNODE}, PayloadServerPid, LogFile) end),
    logge_status("startet", LogFile),
    Pid.

% --------------------------------------------------

loop(Nameservice, PayloadServerPid, LogFile) ->
    receive
        {sendMessage, [StationType, SlotNumber]} -> 
			Payload = request_payload(PayloadServerPid),
			SendTime = vsutil:getUTC(),			
			IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
			Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
            send(Nameservice, Message)
    end,
    loop(Nameservice, PayloadServerPid, LogFile).

send(Nameservice, Message) ->
	timer:sleep(20),
    Nameservice ! {multicast, Message}.
	
	
request_payload(PayloadServerPid) ->
    PayloadServerPid ! {self(), getNextPayload},
    receive
        {payload, Payload} ->
            Payload
    end.

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

    