-module(payloadserver).
-export([
	start/2, 
	send/1,
	loop/1
]).

-define(SERVERNAME, payloadserver).

-define(TEAMNUMBER, "6").
-define(PRAKTIKUMSNUMBER, "2").

start(CoreNode, LogFile) ->
	CoreNodeString = atom_to_list(CoreNode),
	PipeNode = (lists:sublist(LogFile, 1) ++ "-pipe"),
		 
	ServerPid = spawn(fun() -> loop(LogFile) end),
	register(payloadserver, ServerPid),

	CommandString = "java vessel3.Vessel " ++ ?TEAMNUMBER ++ " " ++ ?PRAKTIKUMSNUMBER ++ 
					" | erl -sname " ++ PipeNode ++ " -noshell -s payloadserver send " ++ CoreNodeString ++ " " ++ LogFile,
	VesselPid = spawn(fun() ->
			os:cmd(CommandString)
		 end),
	logge_status("PayloadserverPID: ~p // Vessel3 with Send Pipe PID: ~p", [self(), VesselPid], LogFile),
	ServerPid.

send([CoreNode, LogFile]) ->
	logge_status("send Startet", LogFile),
	case net_adm:ping(CoreNode) of
		pang ->
			logge_status("Couldn't find Payloadserver!", LogFile);
		pong ->
			logge_status("Found Payloadserver: ~p", [CoreNode], LogFile),
			ServerPid = {?SERVERNAME, CoreNode},
			send_(ServerPid)
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
			logge_status("Next Payload to: ~p", [AbsenderPid], LogFile),
			receive
				Payload ->
					logge_status("Sending Payload to: ~p", [AbsenderPid], LogFile),
					%io:fwrite("Got Request from ~p", [AbsenderPid]),
					AbsenderPid ! {payload, Payload}
			end;
		_Any ->
			nothing%logge_status("got: ~p\n", [Any], LogFile)
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