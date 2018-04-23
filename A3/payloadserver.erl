-module(payloadserver).
-export([start/0, send/0, loop/0]).

-define(SERVERIP, {payloadserver, 'payloadserver@Michael-X250'}).

start() ->
	ServerPid = spawn(fun() -> loop() end),
	register(payloadserver, ServerPid),
	timer:sleep(timer:seconds(1)),
	VesselPid = spawn(fun() ->
			os:cmd("java vessel3.Vessel 3 1 | erl -sname test -noshell -s payloadserver send")
		 end),
	io:fwrite("PayloadserverPID: ~p // Vessel3 with Send Pipe PID: ~p\n", [self(), VesselPid]).

send() ->
	PingResult = net_adm:ping('payloadserver@Michael-X250'),
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
					io:fwrite("Got Request from ~p", [AbsenderPid]),
					AbsenderPid ! Payload
			end;
		Any ->
			io:fwrite("got: ~p\n", [Any])
	end,
	loop().