-module(utcclock).

-export([
    start/4,

    adjust/4,

    new_frame_started/2,
    get_current_time/1,
    calc_slot_mid_this_frame_time/3,
    set_alarm/4
]).


-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(OffsetMS, CorePid, StationName, LogFile) ->
    logge_status("Starting with Offset: ~p", [OffsetMS], LogFile),
    TransportTupel = {StationName, 0, 0},
    ClockPid = spawn(fun() -> loop(OffsetMS, CorePid, TransportTupel, LogFile) end),
    ClockPid.

% --------------------------------------------------

loop(OffsetMS, CorePid, TransportTupel, LogFile) ->
    receive
        {adjust, Messages} ->
            {NewOffsetMS, NewTransportTupel} = adjust(OffsetMS, Messages, TransportTupel, LogFile),
            loop(NewOffsetMS, CorePid, NewTransportTupel, LogFile);
        {calcslotmid, FrameStart, SlotNumber, SenderPid} ->
            SendtimeMS = calc_slot_mid_this_frame_time(FrameStart, SlotNumber, LogFile),
            SenderPid ! {resultslotmid, SendtimeMS},
            logge_status("calc got: ~p ~p = ~p", [FrameStart, SlotNumber, SendtimeMS], LogFile),
            loop(OffsetMS, CorePid, TransportTupel, LogFile);
        {alarm, AlarmMessage, TimeWhenItsDue, SenderPid} ->
            TimeTillItsDue = TimeWhenItsDue - get_current_time(OffsetMS),
            set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile),
            loop(OffsetMS, CorePid, TransportTupel, LogFile);
        {calcdifftime, UTCTime, SenderPid} ->
            CurrentTime = get_current_time(OffsetMS),
            DiffTime = CurrentTime - UTCTime,
            SenderPid ! {resultdifftime, DiffTime},
            loop(OffsetMS, CorePid, TransportTupel, LogFile);
        {getcurrenttime, SenderPid} ->
            SenderPid ! {currenttime, get_current_time(OffsetMS)},
            loop(OffsetMS, CorePid, TransportTupel, LogFile);

        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(OffsetMS, CorePid, TransportTupel, LogFile);
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(OffsetMS, CorePid, TransportTupel, LogFile)
    end.

% --------------------------------------------------

adjust(OffsetMS, Messages, TransportTupel, LogFile) ->
    {AverageDiffMS, NewTransportTupel} = calc_average_diff_ms(Messages, OffsetMS, TransportTupel, LogFile),
    NewOffsetMS = round(OffsetMS - AverageDiffMS),
    logge_status("New Offset: ~p (Old: ~p)", [NewOffsetMS, OffsetMS], LogFile),
    {NewOffsetMS, NewTransportTupel}.

calc_average_diff_ms(Messages, OffsetMS, TransportTupel, LogFile) ->
    {TotalDiffMS, TotalCount, NewTransportTupel} = calc_average_diff_ms(Messages, 0, 0, OffsetMS, TransportTupel, LogFile),
    case TotalCount of
        0 ->
            {0, NewTransportTupel};
        TotalCountBigger0 ->
            {TotalDiffMS / TotalCountBigger0, NewTransportTupel}
    end.

calc_average_diff_ms([], TotalDiffMS, TotalCount, _OffsetMS, TransportTupel, _LogFile) ->
    {TotalDiffMS, TotalCount, TransportTupel};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount, OffsetMS, TransportTupel, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            RecvTime = messagehelper:get_receivedtime(CurrentMessage) + OffsetMS,
            TmpDiffMS = RecvTime - SendTime,

            {NewTransportTupel, AverageTransportDelay} = adjust_transport_tupel_and_calc_new_average(TransportTupel, CurrentMessage, TmpDiffMS),
            NewTotalDiffMS = TotalDiffMS + (TmpDiffMS - AverageTransportDelay),
            NewTotalCount = TotalCount + 1,

            logge_status("Send (~p) Recv (~p) Diff (~p) Delay (~p)", [SendTime, RecvTime, TmpDiffMS, AverageTransportDelay], LogFile),
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, NewTotalCount, OffsetMS, NewTransportTupel, LogFile);
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, OffsetMS, TransportTupel, LogFile)
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

new_frame_started(CurrentTime, CurrentFrameNumber) ->
    TimeElapsedInCurrentFrame = CurrentTime - (CurrentFrameNumber * ?FRAMELENGTHMS),
    TimeElapsedInCurrentFrame >= ?FRAMELENGTHMS.

get_current_time(OffsetMS) ->
    Result = vsutil:getUTC() + OffsetMS,
    round(Result).

calc_slot_mid_this_frame_time(FrameStart, SlotNumber, _LogFile) ->
    SlotBeginnInMid = round(SlotNumber * ?SLOTLENGTHMS - (?SLOTLENGTHMS / 2)), % - SLOTLENGTHMS because we want the mid of the slottime
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