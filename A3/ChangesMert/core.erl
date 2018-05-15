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
    PayloadServerPid = payloadserver:start(node(), StationNumberString, LogFile),
    SlotFinderPid = slotfinder:start(self(), StationName, LogFile),
    SendPid = sender:start(PayloadServerPid, LogFile),
    ClockPid = utcclock:start(StationName, ClockOffsetMS, self(), LogFile),		
    receiver:start(SlotFinderPid, ClockPid, StationName, LogFile),
	logge_status("Station ~p ist Ready",[StationName], LogFile),
	frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile).
	
%------------------------------------------ FRAME	
frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile) ->   
	FrameStartTime = vsutil:getUTC(),	
	ClockPid ! {getcurrentoffsetms, self()},
	receive
		{offset, OffsetMS} ->
			logge_status("------------Offset ist ~p --------------", [OffsetMS], LogFile),
			timer:sleep(OffsetMS),					
			logge_status("Asking for new Slot", LogFile),
			SlotFinderPid ! getFreeSlotNum,
			receive
				{slotnum, SlotNumFromSlotFinder} ->
						SlotNumber = SlotNumFromSlotFinder
			end
	end,
	logge_status("Starte Frame on: ~p with SN: ~p", [FrameStartTime, SlotNumber], LogFile),
	SendTakenTimeStart = vsutil:getUTC(),
	slot_loop(?SLOTLENGTHMS, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, 1, FrameStartTime, SendTakenTimeStart).	
	
%------------------------------------------ SLOT	
slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot, FrameStartTime, SendTakenTimeStart) ->
    case (SlotNumber == CurrentSlot) of
		true ->
			logge_status("------Writing Message on ~p", [CurrentSlot], LogFile),
			SendPid ! {sendMessage, [StationType, SlotNumber]},		
			logge_status("------Message is out", LogFile),			
			timer:sleep(T),		
			slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime, SendTakenTimeStart);
			
		false ->
			case (CurrentSlot >= 24) of
				true ->
					logge_status("FRAME END", LogFile),
					SendTakenTimeEND = vsutil:getUTC(),
					logge_status("-------------------------TIME TAKE ~p", [SendTakenTimeEND - SendTakenTimeStart], LogFile),
					frame_loop(StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, LogFile);
				false ->
					timer:sleep(T),
					slot_loop(T, StationName, StationType, SlotFinderPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile, CurrentSlot + 1, FrameStartTime, SendTakenTimeStart)
			end
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