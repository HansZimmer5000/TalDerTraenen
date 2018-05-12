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
    SlotFinderPid = slotfinder:start(self(), StationName, LogFile),	
    ReceiverPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), LogFile),
    PayloadServerPid = payloadserver:start(node(), StationNumberString,LogFile),
    frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile).
	
%------------------------------------------ FRAME	
frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile)
	ClockPid ! {getcurrentoffsetms, self()},
	receive
		Offset ->
			timer:sleep(Offset),
			SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName)
			%change method return actual free slot,
			FrameStartTime = vsutil:getUTC(),
			slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, 0, FrameStartTime)
	end.
	
%------------------------------------------ SLOT	
slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
	SlotStartTime = vsutil:getUTC(),
    case (SlotNumber == CurrentSlot) of
		true ->
			case(T == 40) of
				true ->
					IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
					SendtimeMS = vsutil:getUTC(),
					send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS, LogFile);
					%INFO: sender wartet die 20 ms
        false ->
    end,	
	receive %TODO recive arbeitet proaktiv, nicht nach abruf
		{messageFromBC, Message} ->
			ClockPid ! {messageFromBC, Message, FrameStartTime},
			SlotHandlerPid ! {messageFromBC, Message},	%Slots	in Liste einpflegen, nach find slot leeren.
		
		% loop mit rest slotZeit
		SlotBreakTime = vsutil:getUTC(),
		T = (T - (SlotBreakTime - SlotStartTime)),
		slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
	after T ->
		case (CurrentSlot == 24) of
			true ->
				frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile);
			false ->
				slot_loop(40, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime)
 			end	
	end.


send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, _LogFile) ->
    PayloadServerPid ! {self(), getNextPayload},
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