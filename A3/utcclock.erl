-module(utcclock).

-export([
    start/4,

    get_frame_rest_time/4,
    start_timer_getcurrenttime/4,

    adjust/5,

    new_frame_started/2,
    get_current_time/1,
    calc_slot_mid_this_frame_time/3,
    set_alarm/4
]).


-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

% -------------------- Init --------------------------

start(OffsetMS, CorePid, StationName, LogFile) ->
    logge_status("Starting with Offset: ~p", [OffsetMS], LogFile),
    TransportTupel = {StationName, 0, 0},
    ClockPid = spawn(fun() -> 
        TimerTupel = create_checkframe_timer({0, empty}, OffsetMS),
		loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile) 
	end),
    ClockPid.

% -------------------- Loop --------------------------

loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile) ->
    receive
        newframe -> 
            CorePid ! {newframe, get_current_time(OffsetMS), OffsetMS},
            NewTimerTupel = create_checkframe_timer(TimerTupel, OffsetMS),
            loop(OffsetMS, NewTimerTupel, CorePid, TransportTupel, LogFile);
        {newoffsetandtransporttupel, ReceivedOffsetDiff, NewTransportTupel} ->
            TmpNewOffsetMS = OffsetMS - ReceivedOffsetDiff,
	        NewOffsetMS = (OffsetMS + TmpNewOffsetMS) div 2, 
            case NewOffsetMS == OffsetMS of
                false ->
                    NewTimerTupel = create_checkframe_timer(NewOffsetMS, NewOffsetMS);
                true ->
                    NewTimerTupel = TimerTupel
            end,
            logge_status("New Offset: ~p (Old: ~p)", [NewOffsetMS, OffsetMS], LogFile),
            loop(NewOffsetMS, NewTimerTupel, CorePid, NewTransportTupel, LogFile);
        {adjust, Messages} ->
            ClockPid = self(),
            spawn(fun () -> 
                    adjust(OffsetMS, ClockPid, Messages, TransportTupel, LogFile)
                end),
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        {calcslotmid, FrameStart, SlotNumber, SenderPid} ->
            spawn(fun() -> 
                    SendtimeMS = calc_slot_mid_this_frame_time(FrameStart, SlotNumber, LogFile),
                    SenderPid ! {resultslotmid, SendtimeMS},
                    logge_status("calc got: ~p ~p = ~p", [FrameStart, SlotNumber, SendtimeMS], LogFile),
                    loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile)
                end),
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        {alarm, AlarmMessage, TimeWhenItsDue, SenderPid} ->
            spawn(fun() ->
                    TimeTillItsDue = TimeWhenItsDue - get_current_time(OffsetMS),
                    set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile)
                end),
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        {calcdifftime, UTCTime, SenderPid} ->
            spawn(fun() -> 
                    CurrentTime = get_current_time(OffsetMS),
                    DiffTime = CurrentTime - UTCTime,
                    SenderPid ! {resultdifftime, DiffTime},
                    loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile)
                end),
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        {getcurrenttime, SenderPid} ->
            SenderPid ! {currenttime, get_current_time(OffsetMS)},
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
	    {getcurrenttimeandoffset, SenderPid} ->
	        SenderPid ! {currenttimeandoffset, get_current_time(OffsetMS), OffsetMS},
                loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile);
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(OffsetMS, TimerTupel, CorePid, TransportTupel, LogFile)
    end.

% --------------------- Exported Functions -----------
get_frame_rest_time(StationFrameStart, StationFrameStartOffset, ClockPid, _LogFile) ->
    ClockPid ! {getcurrenttimeandoffset, self()},
    receive
	{currenttimeandoffset, StationFrameNow, StationFrameNowOffset} ->
		TimeElapsedInFrame = StationFrameNow - StationFrameStart + (StationFrameStartOffset - StationFrameNowOffset),
		RestFrameTime = 1000 - TimeElapsedInFrame,
		RestFrameTime
    end.

% ---------------------Internal Functions ---------------------

create_checkframe_timer({LastFrameStart, CurrentTimer}, CurrentOffsetMS) ->
    CurrentTime = get_current_time(CurrentOffsetMS),
    TimeTillNextFrame = calc_time_till_next_frame(CurrentTime),
    timer:cancel(CurrentTimer),
    {ok, NewTimer} = timer:send_after(TimeTillNextFrame - 1, newframe),
    {LastFrameStart, NewTimer}.

calc_time_till_next_frame(CurrentTime) ->
    TimeElapsedInCurrentFrame = CurrentTime rem 1000,
    TimeTillNextFrame = 1000 - TimeElapsedInCurrentFrame,
    TimeTillNextFrame.

adjust(OffsetMS, ClockPid, Messages, TransportTupel, LogFile) ->
    NewTransportTupel = adjust_transport_tupel(Messages, OffsetMS, TransportTupel, LogFile),
    AverageTransportDelay = calc_new_transport_delay_average(TransportTupel),
    logge_status("TransportDelay is ~p", [AverageTransportDelay], LogFile),
    AverageDiffMS = calc_average_diff_ms(Messages, OffsetMS, AverageTransportDelay, LogFile),
    ClockPid ! {newoffsetandtransporttupel, AverageDiffMS, NewTransportTupel}.

adjust_transport_tupel([], _OffsetMS, TransportTupel, _LogFile) ->
    TransportTupel;
adjust_transport_tupel(Messages, OffsetMS, TransportTupel, LogFile) ->
    [CurrentMessage | RestMessages] = Messages,
    {StationName, TransportDelayTotal, TransportCount} = TransportTupel,
    %logge_status("My Name: ~p, other Name: ~p", [StationName, messagehelper:get_station_name(CurrentMessage)], LogFile),
    case StationName == messagehelper:get_station_name(CurrentMessage) of
        true ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            RecvTime = messagehelper:get_receivedtime(CurrentMessage),
            TmpDiffMS = (RecvTime - SendTime),
            NewTransportDelayTotal = TransportDelayTotal + TmpDiffMS,
            NewTransportCount = TransportCount + 1,
            NewTransportTupel = {StationName, NewTransportDelayTotal, NewTransportCount},
            adjust_transport_tupel(RestMessages, OffsetMS, NewTransportTupel, LogFile);
        false -> 
            adjust_transport_tupel(RestMessages, OffsetMS, TransportTupel, LogFile)
    end.

calc_average_diff_ms(Messages, OffsetMS, AverageTransportDelay, LogFile) ->
    {TotalDiffMS, TotalCount} = calc_average_diff_ms(Messages, 0, 0, OffsetMS, AverageTransportDelay, LogFile),
    case TotalCount of
        0 ->
            0;
        TotalCountBigger0 ->
            TotalDiffMS div TotalCountBigger0
    end.

calc_average_diff_ms([], TotalDiffMS, TotalCount, _OffsetMS, _AverageTransportDelay, _LogFile) ->
    {TotalDiffMS, TotalCount};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount, OffsetMS, AverageTransportDelay, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            TmpDiffMS = calc_diff_ms(CurrentMessage, OffsetMS, LogFile),

            NewTotalDiffMS = TotalDiffMS + (TmpDiffMS - AverageTransportDelay),
            NewTotalCount = TotalCount + 1,

            logge_status("Diff (~p)", [TmpDiffMS], LogFile),
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, NewTotalCount, OffsetMS, AverageTransportDelay, LogFile);
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, OffsetMS, AverageTransportDelay, LogFile)
    end.

calc_diff_ms(Message, _OffsetMS, LogFile) ->
    SendTime = messagehelper:get_sendtime(Message),
    RecvTime = messagehelper:get_receivedtime(Message),
    logge_status("Send ~p Recv ~p", [SendTime, RecvTime], LogFile),
    TmpDiffMS = RecvTime - SendTime,
    TmpDiffMS.

calc_new_transport_delay_average(TransportTupel) ->
    {_StationName, TransportDelayTotal, TransportCount} = TransportTupel,
    case TransportCount of
        0 -> AverageTransportDelay = 0;
        _ -> AverageTransportDelay = round(TransportDelayTotal / TransportCount) %Not using 'div' because 'div' is always rounding down
    end,
    AverageTransportDelay.

new_frame_started(CurrentTime, CurrentFrameNumber) ->
    TimeElapsedInCurrentFrame = CurrentTime - (CurrentFrameNumber * ?FRAMELENGTHMS),
    TimeElapsedInCurrentFrame >= ?FRAMELENGTHMS.

get_current_time(OffsetMS) ->
    Result = vsutil:getUTC() + OffsetMS,
    Result.

calc_slot_mid_this_frame_time(FrameStart, SlotNumber, _LogFile) ->
    SlotBeginnInMid = SlotNumber * ?SLOTLENGTHMS - (?SLOTLENGTHMS div 2), % - SLOTLENGTHMS/2 because we want the mid of the slottime
    %logge_status("FrameBeginn: ~p, SlotMid: ~p", [FrameBeginnTime, SlotBeginnInMid], LogFile),
    FrameStart + SlotBeginnInMid.

set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile) -> 
    %TODO Muss das noch genauer da ja ggf. die Uhr sich zu sehr snychornisiert?
    case TimeTillItsDue > 0 of
        true ->
            timer:send_after(TimeTillItsDue, SenderPid, AlarmMessage);
        false ->
            SenderPid ! AlarmMessage,
            logge_status("alarm '~s' with ~pms already too late!", [AlarmMessage, TimeTillItsDue], LogFile)
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







start_timer_getcurrenttime(StationFrameStart, StationFrameStartOffset, ClockPid, LogFile) ->
    ClockPid ! {getcurrenttimeandoffset, self()},
    receive
        {currenttimeandoffset, StationFrameNow, StationFrameNowOffset} ->
            logge_status("start_timer_getcurrenttime with: ~p", [[StationFrameStart, StationFrameStartOffset, StationFrameNow, StationFrameNowOffset]], LogFile),
            _StartTime = vsutil:getUTC(),
            TimeElapsedInFrame = StationFrameNow - StationFrameStart + (StationFrameStartOffset - StationFrameNowOffset),
            RestFrameTime = 1000 - TimeElapsedInFrame,
            case RestFrameTime of
                RestFrameTime when RestFrameTime > 0 ->
                    timer:send_after(RestFrameTime - 1, ClockPid, {getcurrenttimeandoffset, self()});
                _Else ->
                    ClockPid ! {getcurrenttimeandoffset, self()}
            end
    end.