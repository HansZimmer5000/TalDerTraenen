-module(payloadserver).
-export([
	start/1, 
	send/1, 
	loop/1
]).

-define(SERVERNODE, 'payloadserver@Michael-X250').
-define(SERVERIP, {payloadserver, ?SERVERNODE}).

-define(TEAMNUMBER, "3").
-define(PRAKTIKUMSNUMBER, "2").

start(LogFile) ->
	ServerPid = spawn(fun() -> loop(LogFile) end),
	register(payloadserver, ServerPid),
	VesselPid = spawn(fun() ->
			os:cmd("java vessel3.Vessel "++ ?TEAMNUMBER ++ " " ++ ?PRAKTIKUMSNUMBER ++ " | erl -sname test -noshell -s payloadserver send")
		 end),
	logge_status("PayloadserverPID: ~p // Vessel3 with Send Pipe PID: ~p\n", [self(), VesselPid], LogFile),
	ServerPid.

send(LogFile) ->
	PingResult = net_adm:ping(?SERVERNODE),
	case PingResult of
		pang ->
			logge_status("Couldn't find Payloadserver!", LogFile);
		pong ->
			timer:sleep(timer:seconds(1)),
			logge_status("Could find Payloadserver!", LogFile),
			send_(?SERVERIP)
	end.

send_(PayloadServerPid) ->
	Text = io:get_chars('', 24),
	PayloadServerPid ! Text,
	send_(PayloadServerPid).

% Bekommt alle Payloads, verwirft sie direkt ausser:
% Wenn aktueller Payload angefragt bekommt der Absender den nächsten empfangenen Payload
loop(LogFile) ->
	receive
		{AbsenderPid, getNextPayload} ->
			receive
				Payload ->
					%io:fwrite("Got Request from ~p", [AbsenderPid]),
					AbsenderPid ! {payload, Payload}
			end;
		Any ->
			logge_status("got: ~p\n", [Any], LogFile)
	end,
	loop(LogFile).

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p PYLD ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).