-module(core).

-export([
    start/7,

    frame_loop/6
]).

-define(CLOCKOFFSETMS, 0).

% ------------------ Init --------------
start(StationType, StationName, ClockOffsetMS, InterfaceAddress, McastAddress, ReceivePortAtom, LogFile) ->
    ReceivePort = erlang:list_to_integer(atom_to_list(ReceivePortAtom)),
    CorePid = self(),
    Pids = start_other_components(StationName, CorePid, InterfaceAddress, McastAddress, ReceivePort, ClockOffsetMS, LogFile),
    {_RecvPid, _SendPid, ClockPid, _SlotFinderPid, _PayloadServerPid} = Pids,

    ClockPid ! {getcurrenttime, CorePid},
    receive
    	{currenttime, Now} ->
		%MaschineTime or StationTime? Really important?
		WaitingTimeTillFirstFrame = ((1000 - Now) rem 1000) + 1000,
		timer:send_after(WaitingTimeTillFirstFrame - 1, ClockPid, {getcurrenttimeandoffset, CorePid})
    end,
    frame_loop(StationName, StationType, 0, false, Pids, LogFile).

start_other_components(StationName, CorePid, InterfaceAddress, McastAddress, ReceivePort, ClockOffsetMS, LogFile) ->
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    SendPid = sender:start(InterfaceAddress, McastAddress, ReceivePort, ClockPid, LogFile),
    PayloadServerPid = payloadserver:start(LogFile),
    SlotFinderPid = slotfinder:start(CorePid, StationName, LogFile),
    RecvPid = receiver:start(self(), ClockPid, SlotFinderPid, StationName, InterfaceAddress, McastAddress, ReceivePort,LogFile),
    {RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid}.

%---------------- Loop ----------------

frame_loop(StationName, StationType, CurrentSlotNumber, InSendphase, Pids, LogFile) ->
    {RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid} = Pids,
    
    CorePid = self(),

    receive 
        {currenttimeandoffset, StationFrameStart, StationFrameStartOffset} ->  
            logge_status("New Frame Started at ~p with Slot ~p --------", [StationFrameStart, CurrentSlotNumber], LogFile),
            SlotFinderPid ! newframe,
            % Will start concurrent start_sending_process if insendphase = true
            check_insendphase_and_start_sending_process(InSendphase, SendPid, StationFrameStart, CurrentSlotNumber, StationType, CorePid, ClockPid, SlotFinderPid, PayloadServerPid, LogFile),
		
    	    ClockpifStartTime = vsutil:getUTC(),
            % Will concurrently listen to the received messages and eventually (around frame end) will return {stationwasinvolved, Bool}
            RecvPid ! listentoframe,

            ReturnValue = utcclock:get_frame_rest_time(StationFrameStart, StationFrameStartOffset, ClockPid, LogFile),
	    logge_status("RestFrameTime = ~p", [ReturnValue], LogFile),
	    case ReturnValue of
		RestFrameTime when RestFrameTime > 0 ->
			continue;
		_Else ->
			RestFrameTime = 0
            end,
            
            % Will check in which Station is and will act accordingly (Entry- or Sendphase)
            {NextInSendphase, NextSlotNumber} = check_insendphase_and_return_nextinsendphase_and_nextslotnumber(InSendphase, CorePid, SlotFinderPid, RestFrameTime, LogFile),

	    logge_status("Before Sleep need ~p ms", [ vsutil:getUTC() - ClockpifStartTime], LogFile),
	    utcclock:start_timer_getcurrenttime(StationFrameStart, StationFrameStartOffset, ClockPid, LogFile),

	    %logge_status("After Sleep ~p ms", [ vsutil:getUTC() - ClockpifStartTime], LogFile),
	    % Danger! Should be 1000 but if its not something is terrible wrong or its:
	    %Changes in Offset from Beginn to End are not recognized!
	    FrameTotalTime = 1000 - utcclock:get_frame_rest_time(StationFrameStart, StationFrameStartOffset, ClockPid, LogFile),

            logge_status(
                "Frame Ended after ~p  with NextInSendphase = ~p", 
                [FrameTotalTime, NextInSendphase], LogFile),
            %ClockPid ! {getcurrenttimeandoffset, CorePid},
            frame_loop(StationName, StationType, NextSlotNumber, NextInSendphase, Pids, LogFile)

        after timer:seconds(1) ->
            logge_status("Currenttime from Clock not within 1 second", LogFile),
            frame_loop(StationName, StationType, 0, false, Pids, LogFile)
    end.

%---------------- Internal Functions ---------------
check_insendphase_and_start_sending_process(InSendphase, SendPid, StationFrameStart, CurrentSlotNumber, StationType, CorePid, ClockPid, SlotFinderPid, PayloadServerPid, LogFile) -> 
    case InSendphase of
        true ->
            sender:start_sending_process(CorePid, SendPid, StationFrameStart, CurrentSlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile);
        false ->
            continue
    end.

check_insendphase_and_return_nextinsendphase_and_nextslotnumber(InSendphase, CorePid, SlotFinderPid, RestFrameTime, LogFile) -> 
    case InSendphase of
            false ->
                % Entryphase
		logge_status("Schlafe ~pms", [RestFrameTime - 2], LogFile),
		timer:sleep(RestFrameTime - 2),
                {NextInSendphase, NextSlotNumber} = get_nextinsendphase_and_nextslotnumber(CorePid, SlotFinderPid, RestFrameTime, LogFile);
            true -> 
                % Sendphase
                {NextInSendphase, NextSlotNumber} = handle_sendphase_messages(RestFrameTime, LogFile)
    end,
    {NextInSendphase, NextSlotNumber}.

get_nextinsendphase_and_nextslotnumber(CorePid, SlotFinderPid, RestFrameTime, LogFile) ->
    StartTime = vsutil:getUTC(),
    SlotFinderPid ! {getFreeSlotNum, CorePid},
    receive 
        {slotnum, NextSlotNumber} -> 
            NextInSendphase = true
        after RestFrameTime -> 
            NextSlotNumber = 0,
            NextInSendphase = false,
            logge_status("Never received Slotnumber", LogFile)
    end,
    logge_status("Needed ~pms for getFreeSlotNum", [vsutil:getUTC() - StartTime], LogFile),
    {NextInSendphase, NextSlotNumber}.

handle_sendphase_messages(RestFrameTime, LogFile) ->
    StartTime = vsutil:getUTC(),
    receive
            {messagewassend, MessageWasSend, ReceivedNextSlotNumber} -> 
		NewRestFrameTime = RestFrameTime - (vsutil:getUTC() - StartTime),
		logge_status("NewRestFrameTime ~p", [NewRestFrameTime], LogFile),
                {NextInSendphase, NextSlotNumber} = wait_for_stationwasinvolved_and_return_nextinsendphase_and_nextslotnumber(MessageWasSend, ReceivedNextSlotNumber, NewRestFrameTime, LogFile)
            after RestFrameTime ->
                logge_status("Messagewassend was never received", LogFile),
                NextInSendphase = false,
                NextSlotNumber = 0
    end,
    {NextInSendphase, NextSlotNumber}.

wait_for_stationwasinvolved_and_return_nextinsendphase_and_nextslotnumber(MessageWasSend, ReceivedNextSlotNumber, RestFrameTime, LogFile) ->
    receive 
        {stationwasinvolved, true} ->
            	logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[true, MessageWasSend], LogFile),
		NextInSendphase = false,
		NextSlotNumber = 0
        after RestFrameTime - 1 -> 
             	logge_status("stationwasinvolved = true was never received", LogFile),
		case MessageWasSend of
			true ->
				NextInSendphase = true,
				NextSlotNumber = ReceivedNextSlotNumber;
			false ->
				NextInSendphase = false,
				NextSlotNumber = 0
		end
    end,
    {NextInSendphase, NextSlotNumber}.
%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p Core ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).