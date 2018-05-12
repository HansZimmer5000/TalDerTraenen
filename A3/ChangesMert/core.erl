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
    PayloadServerPid = "",
	logge_status("Station ist Ready", LogFile),
	frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile).
	
%------------------------------------------ FRAME	
frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile) ->   
	ClockPid ! {getcurrentoffsetms, self()},
	receive
		Offset ->
			timer:sleep(Offset),
			SlotFinderPid ! {getFreeSlotNum},
			receive
				SlotNumFromSlotFinder ->
					SlotNumber = SlotNumFromSlotFinder
					
			end,
			FrameStartTime = vsutil:getUTC(),			
			logge_status("Starte Frame on: ~p with SN: ~p", [FrameStartTime, SlotNumber], LogFile),
			slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, 0, FrameStartTime)
	end.	
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
					%send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS, LogFile)					
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
			ClockPid ! {messageFromBC, Message, FrameStartTime},
			SlotFinderPid ! {messageFromBC, Message},		
			% loop mit rest slotZeit
			SlotBreakTime = vsutil:getUTC(),
			T = (T - (SlotBreakTime - SlotStartTime)),
			slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime)
	after T ->
		logge_status("neuer Slot beginnt ~p", [CurrentSlot], LogFile),
		case (CurrentSlot >= 24) of
			true ->
				logge_status("FRAME END", LogFile),
				frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile);
			false ->
				slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime)
		end
	end.	




send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, _LogFile) ->
    Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, ""),
    SendPid ! {send, Message}.
	
%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p Core ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).