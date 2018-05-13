-module(utcclock).

-export([
    start/3,

    adjust/4,
	calcOffSet/3,

    check_frame/4,
    new_frame_started/2,
    get_current_time/2,
    calc_slot_beginn_this_frame_time/2,
    set_alarm/3,
    calc_diff_time/2
]).

-define(FRAMECHECKCYCLEMS, 10).
-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(OffsetMS, CorePid, LogFile) ->
    Starttime = vsutil:getUTC(),
    logge_status("Starttime: ~p with Offset: ~p", [Starttime, OffsetMS], LogFile),
    FramecheckCycleMS = ?FRAMECHECKCYCLEMS,
    CurrentFrameNumber = 0,
    ClockPid = spawn(fun() -> loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile) end),
    ClockPid.

% --------------------------------------------------

loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile) ->
    receive
        {getcurrentoffsetms, SenderPid} ->
		
            logge_status("Got offset request", LogFile),
            SenderPid ! {offset, OffsetMS},
            loop(Starttime, 0, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile);
			
		{messageFromBC, Message, FrameStartTime} ->
			NewOffsetMS = calcOffSet(Message, OffsetMS, FrameStartTime),
			loop(Starttime, NewOffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile);	
			
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid, LogFile)
    end.
	
	
calcOffSet(CurrentMessage, CurrentOffSet, FrameStartTime) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
			SlotNumber = messagehelper:get_slotnumber(CurrentMessage) - 1,
            NewOffSet = (FrameStartTime - (SendTime - ((SlotNumber * 40) + 20))),
			case NewOffSet < 0 of
				true ->
					NewOffSet * (-1);
				false ->
					NewOffSet
			end
    end.

	
	
adjust(Starttime, OffsetMS, Messages, LogFile) ->
    AverageDiffMS = calc_average_diff_ms(Messages, Starttime, OffsetMS, LogFile),
    NewOffsetMS = OffsetMS - AverageDiffMS,
    round(NewOffsetMS).

calc_average_diff_ms(Messages, Starttime, OffsetMS, LogFile) ->
    {TotalDiffMS, TotalCount} = calc_average_diff_ms(Messages, 0, 0, Starttime, OffsetMS, LogFile),
    case TotalCount of
        0 ->
            0;
        TotalCountBigger0 ->
            TotalDiffMS / TotalCountBigger0
    end.


calc_average_diff_ms([], TotalDiffMS, TotalCount, _Starttime, _OffsetMS, _LogFile) ->
    {TotalDiffMS, TotalCount};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount, Starttime, OffsetMS, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            RecvTime = messagehelper:get_receivedtime(CurrentMessage) - Starttime + OffsetMS,
            NewTotalDiffMS = TotalDiffMS + RecvTime - SendTime - 150, %150 is about Transporttime
            logge_status("Send (~p) Recv (~p) Total (~p) Count(~p)", [SendTime, RecvTime, NewTotalDiffMS, TotalCount + 1], LogFile),
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, TotalCount + 1, Starttime, OffsetMS, LogFile);
        Any ->
            logge_status("Unkown Station: ~p", [Any], LogFile),
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, Starttime, OffsetMS, LogFile)
    end.

check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid) ->   
    CurrentTime = get_current_time(Starttime, OffsetMS),
    case new_frame_started(CurrentTime, CurrentFrameNumber) of
        true ->
            CorePid ! newframe,
            %logge_status(io_lib:format("New Frame at: ~p", [CurrentTime])),
            CurrentFrameNumber + 1;
        false ->
            %logge_status(io_lib:format("No New Frame at: ~p", [CurrentTime])),
            CurrentFrameNumber
    end.

new_frame_started(CurrentTime, CurrentFrameNumber) ->
    TimeElapsedInCurrentFrame = CurrentTime - (CurrentFrameNumber * ?FRAMELENGTHMS),
    TimeElapsedInCurrentFrame >= ?FRAMELENGTHMS.

get_current_time(Starttime, OffsetMS) ->
    Result = vsutil:getUTC() - Starttime + OffsetMS,
    round(Result).

calc_slot_beginn_this_frame_time(CurrentFrameNumber, SlotNumber) ->
    FrameBeginnTime = CurrentFrameNumber * ?FRAMELENGTHMS,
    SlotBeginnInFrame = SlotNumber * ?SLOTLENGTHMS - ?SLOTLENGTHMS, % - SLOTLENGTHMS because we want the start of the slottime
    FrameBeginnTime + SlotBeginnInFrame.

set_alarm(AlarmMessage, TimeTillItsDue, SenderPid) -> 
    %TODO Muss das noch genauer da ja ggf. die Uhr sich zu sher snychornisiert?
    case TimeTillItsDue > 0 of
        true ->
            timer:send_after(TimeTillItsDue, SenderPid, AlarmMessage);
        false ->
            SenderPid ! AlarmMessage
    end.

calc_diff_time(CurrentTime, SlotBeginnInFrame) ->
    CurrentTime - SlotBeginnInFrame.

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p -- Clock ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).