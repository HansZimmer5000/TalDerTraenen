-module(utcclock).

-export([
    start/4,

    adjust/5,

    check_frame/4,
    new_frame_started/2,
    get_current_time/2,
    calc_slot_beginn_this_frame_time/2,
    set_alarm/4
]).

-define(FRAMECHECKCYCLEMS, 10).
-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(OffsetMS, CorePid, StationName, LogFile) ->
    Starttime = vsutil:getUTC(),
    logge_status("Starttime: ~p with Offset: ~p", [Starttime, OffsetMS], LogFile),
    CurrentFrameNumber = 0,
    TransportTupel = {StationName, 0, 0},
    ClockPid = spawn(fun() -> loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile) end),
    _CheckPid = spawn(fun() -> check_frame_loop(ClockPid) end),
    ClockPid.

% --------------------------------------------------
check_frame_loop(ClockPid) ->
    timer:sleep(?FRAMECHECKCYCLEMS),
    ClockPid ! checkframe,
    check_frame_loop(ClockPid).

loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile) ->
    receive
        {adjust, Messages} ->
            {NewOffsetMS, NewTransportTupel} = adjust(Starttime, OffsetMS, Messages, TransportTupel, LogFile),
            loop(Starttime, NewOffsetMS, CurrentFrameNumber, CorePid, NewTransportTupel, LogFile);
        checkframe ->
            NewCurrentFrameNumber = check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid),
            loop(Starttime, OffsetMS, NewCurrentFrameNumber, CorePid, TransportTupel, LogFile);
        {calcslotbeginn, SlotNumber, SenderPid} ->
            SendtimeMS = calc_slot_beginn_this_frame_time(CurrentFrameNumber, SlotNumber),
            SenderPid ! {resultslotbeginn, SendtimeMS},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile);
        {alarm, AlarmMessage, TimeWhenItsDue, SenderPid} ->
            TimeTillItsDue = TimeWhenItsDue - get_current_time(Starttime, OffsetMS),
            set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile),
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile);
        {calcdifftime, SlotBeginnInFrame, SenderPid} ->
            CurrentTime = get_current_time(Starttime, OffsetMS),
            DiffTime = CurrentTime - SlotBeginnInFrame,
            SenderPid ! {resultdifftime, DiffTime},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile);
        {getcurrenttime, SenderPid} ->
            SenderPid ! {currenttime, get_current_time(Starttime, OffsetMS)},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile);

        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile);
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, TransportTupel, LogFile)
    end.

% --------------------------------------------------

adjust(Starttime, OffsetMS, Messages, TransportTupel, LogFile) ->
    {AverageDiffMS, NewTransportTupel} = calc_average_diff_ms(Messages, Starttime, OffsetMS, TransportTupel, LogFile),
    NewOffsetMS = round(OffsetMS - AverageDiffMS),
    logge_status("New Offset: ~p (Old: ~p)", [NewOffsetMS, OffsetMS], LogFile),
    {NewOffsetMS, NewTransportTupel}.

calc_average_diff_ms(Messages, Starttime, OffsetMS, TransportTupel, LogFile) ->
    {TotalDiffMS, TotalCount, NewTransportTupel} = calc_average_diff_ms(Messages, 0, 0, Starttime, OffsetMS, TransportTupel, LogFile),
    case TotalCount of
        0 ->
            {0, NewTransportTupel};
        TotalCountBigger0 ->
            {TotalDiffMS / TotalCountBigger0, NewTransportTupel}
    end.

calc_average_diff_ms([], TotalDiffMS, TotalCount, _Starttime, _OffsetMS, TransportTupel, _LogFile) ->
    {TotalDiffMS, TotalCount, TransportTupel};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount, Starttime, OffsetMS, TransportTupel, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            RecvTime = messagehelper:get_receivedtime(CurrentMessage) - Starttime + OffsetMS,
            TmpDiffMS = RecvTime - SendTime,

            {NewTransportTupel, AverageTransportDelay} = adjust_transport_tupel_and_calc_new_average(TransportTupel, CurrentMessage, TmpDiffMS),
            NewTotalDiffMS = TotalDiffMS + (TmpDiffMS - AverageTransportDelay),
            NewTotalCount = TotalCount + 1,

            logge_status("Send (~p) Recv (~p) Diff (~p) Delay (~p)", [SendTime, RecvTime, TmpDiffMS, AverageTransportDelay], LogFile),
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, NewTotalCount, Starttime, OffsetMS, NewTransportTupel, LogFile);
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, Starttime, OffsetMS, TransportTupel, LogFile)
    end.

adjust_transport_tupel_and_calc_new_average(TransportTupel, CurrentMessage, TmpDiffMS) ->
    {StationName, TransportDelayTotal, TransportCount} = TransportTupel,
    case messagehelper:get_station_name(CurrentMessage) of
        StationName ->
            NewTransportDelayTotal = TransportDelayTotal + TmpDiffMS,
            NewTransportCount = TransportCount + 1;
        _Any ->
            NewTransportDelayTotal = TransportDelayTotal,
            NewTransportCount = TransportCount
    end,
    NewTransportTupel = {StationName, NewTransportDelayTotal, NewTransportCount},

    case NewTransportCount of
        0 -> AverageTransportDelay = 0;
        _ -> AverageTransportDelay = round(NewTransportDelayTotal / NewTransportCount)
    end,
    {NewTransportTupel, AverageTransportDelay}.

check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid) ->   
    CurrentTime = get_current_time(Starttime, OffsetMS),
    case new_frame_started(CurrentTime, CurrentFrameNumber) of
        true ->
            CorePid ! newframe,
            CurrentFrameNumber + 1;
        false ->
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

set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile) -> 
    %TODO Muss das noch genauer da ja ggf. die Uhr sich zu sehr snychornisiert?
    case TimeTillItsDue > 0 of
        true ->
            timer:send_after(TimeTillItsDue, SenderPid, AlarmMessage);
        false ->
            SenderPid ! AlarmMessage,
            logge_status("~s mit ~p zu spaet!", [AlarmMessage, TimeTillItsDue], LogFile)
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