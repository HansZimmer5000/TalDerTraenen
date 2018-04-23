-module(payloadserver).
-export([start/0, send/0, loop/0]).

-define(SERVERIP, {payloadserver, 'payloadserver@Michael-X250'}).

start() ->
	ServerPid = spawn(fun() -> loop() end),
	register(payloadserver, ServerPid),
	timer:sleep(timer:seconds(1)),
	VesselPid = spawn(fun() ->
			%os:cmd("erl -sname test -noshell -s payloadserver send")
			%os:cmd("echo hello | erl -sname test -noshell -s payloadserver send")
			os:cmd("java vessel3.Vessel 3 1 | erl -sname test -noshell -s payloadserver send")
		 end),
	io:fwrite("~p/~p", [self(), VesselPid]).

send() ->
	io:fwrite("~p\n", [net_adm:ping('payloadserver@Michael-X250')]),
	timer:sleep(timer:seconds(1)),
	?SERVERIP ! connected,
	send_().
send_() ->
	case io:get_chars('', 24) of
	    eof -> 
			io:fwrite("eof");
		Text ->
			?SERVERIP ! Text,
			io:fwrite("send: ~p", [Text])
	end,
	send_().

% Bekommt alle Payloads, verwirft sie direkt ausser:
% Wenn aktueller Payload angefragt bekommt der Absender den nächsten empfangenen Payload
loop() ->
	receive
		{AbsenderPid, getNextPayload} ->
			receive
				Payload ->
					AbsenderPid ! Payload
			end;
		Any ->
			io:fwrite("got: ~p\n", [Any])
	end,
	loop().