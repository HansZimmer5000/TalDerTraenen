-module(core).

-export([
    start/3,
    start/4
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
	logge_status("Station ist Ready", LogFile),
	frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile).
	
%------------------------------------------ FRAME	
frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile) ->   
	ClockPid ! {getcurrentoffsetms, self()},
	receive
		{offset, OffsetMS} ->
			logge_status("Offset ist ~p", [OffsetMS], LogFile),
			case (OffsetMS > 0) of
				true ->		
					timer:sleep(OffsetMS),					
					logge_status("Asking for new Slot", LogFile),
					SlotFinderPid ! getFreeSlotNum,
					receive
						{slotnum, SlotNumFromSlotFinder} ->
							SlotNumber = SlotNumFromSlotFinder
					end;
				false ->
					SlotFinderPid ! getFreeSlotNum,
					receive
						{slotnum, SlotNumFromSlotFinder} ->
							SlotNumber = SlotNumFromSlotFinder
					end
			end
	end,
	FrameStartTime = vsutil:getUTC(),			
	logge_status("Starte Frame on: ~p with SN: ~p", [FrameStartTime, SlotNumber], LogFile),
	slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, 0, FrameStartTime).	
	
%------------------------------------------ SLOT	
slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime) ->
	SlotStartTime = vsutil:getUTC(),
    case (SlotNumber == CurrentSlot) of
		true ->
			case (T == 40) of
				true ->				
					logge_status("Writing Message on ~p", [CurrentSlot], LogFile),
					IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
					SendtimeMS = vsutil:getUTC(),
					send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS, LogFile),			
					slotReciveloop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime, SendtimeMS);
					
				false ->
					slotReciveloop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime, SlotStartTime)
			end;
		false ->
			slotReciveloop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime, SlotStartTime)
	end.
	
slotReciveloop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime, SlotStartTime) ->
	receive
		{messageFromBC, Message} ->
			ReceivedTime = vsutil:getUTC(),
			ConvertedMessage = messagehelper:convert_message_from_byte(Message, ReceivedTime),
			ClockPid ! {messageFromBC, ConvertedMessage, FrameStartTime},
			SlotFinderPid ! {messageFromBC, ConvertedMessage},		
			% loop mit rest slotZeit			
			SlotBreakTime = vsutil:getUTC(),
			NewT = (T - (SlotBreakTime - SlotStartTime)),	
			case (NewT < 1 ) of
				true ->	
					logge_status("T war MINUS mit ~p",[NewT], LogFile),
					slot_loop(0, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime);
				false ->
					slot_loop(NewT, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
			end
	after T ->
		case (CurrentSlot >= 24) of
			true ->
				logge_status("FRAME END", LogFile),
				frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile);
			false ->
				slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime)
		end
	end.	


send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, LogFile) ->
	Payload = request_payload(PayloadServerPid),
    Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
    SendPid ! {send, Message}.
	
request_payload(PayloadServerPid) ->
    PayloadServerPid ! {self(), getNextPayload},
    receive
        {payload, Payload} ->
            Payload
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