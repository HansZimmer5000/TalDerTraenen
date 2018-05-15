-module(utcclock).

-export([
    start/4
]).

-define(FRAMECHECKCYCLEMS, 10).
-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(StationName, OffsetMS, CorePid, LogFile) ->
    Starttime = vsutil:getUTC(),
    logge_status("Starttime: ~p with Offset: ~p", [Starttime, OffsetMS], LogFile),
    FramecheckCycleMS = ?FRAMECHECKCYCLEMS,
    CurrentFrameNumber = 0,
	FrameStartTime = vsutil:getUTC(),
    ClockPid = spawn(fun() -> loop(StationName, Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, FrameStartTime) end),
    ClockPid.

% --------------------------------------------------

loop(StationName, Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, FrameStartTime) ->
    receive
        {getcurrentoffsetms, SenderPid} ->		
            logge_status("Got offset request", LogFile),
            SenderPid ! {offset, OffsetMS},
			NewFrameStartTime =  vsutil:getUTC() + OffsetMS,
            loop(StationName, Starttime, 0, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, NewFrameStartTime);
			
		{messageFromBC, Message} ->		
            MessageStationName = messagehelper:get_station_name(Message),
			case (MessageStationName == StationName) of
				true ->					
					loop(StationName, Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, FrameStartTime);	
				false ->
					NewOffsetMS = calcOffSet(Message, FrameStartTime, OffsetMS, LogFile),
					loop(StationName, Starttime, NewOffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, FrameStartTime)					
			end;			
			
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(StationName, Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile, FrameStartTime)
    end.
	
	
calcOffSet(CurrentMessage, FrameStartTime, OffsetMS, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            StationName = messagehelper:get_station_name(CurrentMessage),
			SlotNumber = messagehelper:get_slotnumber(CurrentMessage) - 1,
            NewOffSet = round((((FrameStartTime - (SendTime - ((SlotNumber * 31) + 15)))+OffsetMS)/2)),	
            logge_status("Nachricht erhalten vo ~p mit offset ~p",[StationName, NewOffSet], LogFile),			
            case NewOffSet < 0 of
				true ->
					Offset = 0,
					Offset;
				false ->
					NewOffSet
			end
    end.


%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p -- Clock ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).