-module(receiver).

-export([
    start/4,
    start/5,
    
    loop/4
    ]).

-define(NSNODE, hole_wert_aus_config_mit_key(node)).
-define(NSNAME, nameservice).

-define(SLOTLENGTHMS, 40).


start(SlotFinderPid, ClockPid, StationName, LogFile) ->
    start(SlotFinderPid, ClockPid, StationName, LogFile, {?NSNAME, ?NSNODE}).

start(SlotFinderPid, ClockPid, StationName, LogFile, NsPid) ->
    Pid = spawn(fun() -> loop(SlotFinderPid, ClockPid, StationName, LogFile) end),	
	NsPid ! {enlist, Pid},
	logge_status("starte", LogFile),
	Pid.
	
% ------------------------------------------

loop(SlotFinderPid, ClockPid, StationName, LogFile) ->
    receive
        {udp, _Socket0, _Ip0, _Port0, Message} ->
			ReceivedTime = vsutil:getUTC(),
			ConvertedMessage = messagehelper:convert_message_from_byte(Message, ReceivedTime),
			ClockPid ! {messageFromBC, ConvertedMessage},
			SlotFinderPid ! {messageFromBC, ConvertedMessage},
			loop(SlotFinderPid, ClockPid, StationName, LogFile);
        Any -> 
            logge_status("Got: ~p in loop", [Any], LogFile),
			loop(SlotFinderPid, ClockPid, StationName, LogFile)
    end.

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p - Recv ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).

hole_wert_aus_config_mit_key(Key) ->
    {ok, ConfigListe} = file:consult('nameservice.cfg'),
    {ok, Value} = vsutil:get_config_value(Key, ConfigListe),
    Value.

