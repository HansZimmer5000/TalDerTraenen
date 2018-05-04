-module(payloadserver).
-export([start/0, send/0, loop/0]).

-define(SERVERNODE, 'payloadserver@Michael-X250').
-define(SERVERIP, {payloadserver, ?SERVERNODE}).

-define(TEAMNUMBER, "3").
-define(PRAKTIKUMSNUMBER, "2").

start() ->
	ServerPid = spawn(fun() -> loop() end),
	register(payloadserver, ServerPid),
	VesselPid = spawn(fun() ->
			os:cmd("java vessel3.Vessel "++ ?TEAMNUMBER ++ " " ++ ?PRAKTIKUMSNUMBER ++ " | erl -sname test -noshell -s payloadserver send")
		 end),
	io:fwrite("PayloadserverPID: ~p // Vessel3 with Send Pipe PID: ~p\n", [self(), VesselPid]),
	ServerPid.

send() ->
	PingResult = net_adm:ping(?SERVERNODE),
	case PingResult of
		pang ->
			io:fwrite("Couldn't find Payloadserver!");
		pong ->
			timer:sleep(timer:seconds(1)),
			io:fwrite("Could find Payloadserver!"),
			send_(?SERVERIP)
	end.

send_(PayloadServerPid) ->
	Text = io:get_chars('', 24),
	PayloadServerPid ! Text,
	send_(PayloadServerPid).

% Bekommt alle Payloads, verwirft sie direkt ausser:
% Wenn aktueller Payload angefragt bekommt der Absender den nächsten empfangenen Payload
loop() ->
	receive
		{AbsenderPid, getNextPayload} ->
			receive
				Payload ->
					%io:fwrite("Got Request from ~p", [AbsenderPid]),
					AbsenderPid ! Payload
			end;
		Any ->
			io:fwrite("got: ~p\n", [Any])
	end,
	loop().