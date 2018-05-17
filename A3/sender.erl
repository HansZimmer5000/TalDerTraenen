-module(sender).

-export([
    start/5
]).

start(InterfaceAddress,  McastAddressAtom, ReceivePort, ClockPid, LogFile) ->
    Socket = create_socket(InterfaceAddress, ReceivePort),
    Pid = spawn(fun() -> loop(Socket, McastAddressAtom, ReceivePort, ClockPid, LogFile) end),
    logge_status("Sending to ~p:~p", [McastAddressAtom, ReceivePort], LogFile),
    Pid.

create_socket(InterfaceAddress, ReceivePort) ->
    vsutil:openSe(InterfaceAddress, ReceivePort).
    %{ok, Socket} = gen_udp:open(0, [binary]),
    %Socket.
% --------------------------------------------------

loop(Socket, McastAddressAtom, ReceivePort, ClockPid, LogFile) ->
    receive
        {send, IncompleteMessage, Payload} -> 
	    ClockPid ! {getcurrenttime, self()},
	    receive 
		{currenttime, SendTime} -> 
			Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
    			logge_status("Sende Nachricht", LogFile),
			send(Socket, McastAddressAtom, ReceivePort, Message, LogFile)
	    end
    end,
    loop(Socket, McastAddressAtom, ReceivePort, ClockPid, LogFile).

send(Socket, McastAddressAtom, ReceivePort, Message, LogFile) ->
    {ok, McastAddress} = inet_parse:address(atom_to_list(McastAddressAtom)),
    ReturnVal = gen_udp:send(Socket, McastAddress, ReceivePort, Message),
    logge_status("sended with: ~p", [ReturnVal], LogFile).

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p - Send ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).