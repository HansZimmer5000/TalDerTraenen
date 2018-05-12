-module(core).

-export([
    start/3,
    start/4,

    listen_to_frame/1,
    listen_to_slot/1,

    notify_when_preperation_and_send_due/3
]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).
-define(SLOTLENGTHMS, 40).

start(StationType, StationName, LogFile) ->
    start(StationType, StationName, LogFile, ?CLOCKOFFSETMS).

start(StationType, StationName, LogFile, ClockOffsetMS) ->
    StationNumberString = lists:sublist(StationName, 9,2),
    RecvPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), LogFile),
    PayloadServerPid = payloadserver:start(node(), StationNumberString,LogFile),
    frame_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, LogFile).
	
%------------------------------------------ FRAME	
frame_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, LogFile)
	ClockPid ! {getcurrentoffsetms, self()}, % offset = 0 after asking
	FrameStartTime = vsutil:getUTC(),
	receive
		Offset ->
			timer:sleep(Offset),
			SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName)%change method return actual free slot,
			slot_loop(?SLOTLENGTHMS, StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, 0, FrameStartTime)
	end.
	
%------------------------------------------ SLOT
	
slot_loop(T, StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
	{_, _, StartMC} = erlang:timestamp(),
    case (SlotNumber == CurrentSlot) of
		true ->
			IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
			send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS, LogFile); %time soll die methode selber machen beim erstellen des Paketes
            %TODO: nur 1 mal senden (flag)
        false ->
    end,
	
	receive 
		{messageFromBC, Message} ->
			ClockPid ! {messageFromBC, Message, FrameStartTime},
			SlotHandlerPid ! {messageFromBC, Message},	%Slots	in Liste einpflegen, nach find slot leeren.
		
		% loop mit rest slotZeit
		{_, _, EndMC} = erlang:timestamp(),
		T = (T - (EndMC - StartMC)),
		slot_loop(T, StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
	after T ->
		case (CurrentSlot == 24) of
			true ->
				frame_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, LogFile);
			false ->
				slot_loop(40, StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime)
 			end	
	end.


send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, _LogFile) ->
    PayloadServerPid ! {self(), getNextPayload},
    %logge_status("Warte auf Payload", LogFile),
    receive
        {payload, Payload} ->
            %logge_status("payload", LogFile),
            Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
            SendPid ! {send, Message}
    end.
	
%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p Core ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).