-module(payloadserver).
-export([
	start/1, 
	send/2,
	receive_loop/2
]).

-define(SERVERNAME, payloadserver).

-define(TEAMNUMBER, "06").

start(LogFile) ->
	ServerPid = start_payload_server(LogFile),
	VesselPid = start_vessel(ServerPid, LogFile),

	logge_status("PayloadserverPID: ~p // Vessel3 with Send Pipe PID: ~p", [self(), VesselPid], LogFile),
	ServerPid.

start_payload_server(LogFile) ->
	ServerPid = spawn(fun() -> receive_loop("team 00-00-emptydefault-", LogFile) end),
	register(payloadserver, ServerPid),
	ServerPid.

start_vessel(PayloadServerPid, LogFile) ->
	VesselPid = spawn(fun() -> send(PayloadServerPid, LogFile) end),
	VesselPid.

send(PayloadServerPid, LogFile) ->
	Text = io:get_chars('', 24),
	%logge_status("Got Payload: ~p", [Text], LogFile),
	PayloadServerPid ! Text,
	send(PayloadServerPid, LogFile).

% Bekommt alle Payloads, verwirft sie direkt ausser:
% Wenn aktueller Payload angefragt bekommt der Absender den nächsten empfangenen Payload
receive_loop(LastPayload, LogFile) ->
	receive
		{AbsenderPid, getNextPayload} ->
			%logge_status("Got getNextPayload", LogFile),
			AbsenderPid ! {payload, LastPayload},
		 	receive_loop(LastPayload, LogFile);
		Payload ->
			receive_loop(Payload, LogFile)
	end.

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ---- PYLD ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).