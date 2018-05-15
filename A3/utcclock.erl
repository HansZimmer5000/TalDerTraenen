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
    NewTransportTupel = adjust_transport_tupel(Messages, OffsetMS, TransportTupel, LogFile),
    AverageTransportDelay = 0,%calc_new_transport_delay_average(TransportTupel),
    AverageDiffMS = calc_average_diff_ms(Messages, OffsetMS, AverageTransportDelay, LogFile),
    NewOffsetMS = round(OffsetMS - AverageDiffMS),
    logge_status("New Offset: ~p (Old: ~p) | New Delay: ~p", [NewOffsetMS, OffsetMS, AverageTransportDelay], LogFile),
    {NewOffsetMS, NewTransportTupel}.

adjust_transport_tupel([], _OffsetMS, TransportTupel, _LogFile) ->
    TransportTupel;
adjust_transport_tupel(Messages, OffsetMS, TransportTupel, LogFile) ->
    [CurrentMessage | RestMessages] = Messages,
    {StationName, TransportDelayTotal, TransportCount} = TransportTupel,
    %logge_status("My Name: ~p, other Name: ~p", [StationName, messagehelper:get_station_name(CurrentMessage)], LogFile),
    case StationName == messagehelper:get_station_name(CurrentMessage) of
        true ->
            SendTime = messagehelper:get_sendtime(CurrentMessage),
            RecvTime = messagehelper:get_receivedtime(CurrentMessage) + OffsetMS,
            TmpDiffMS = round((RecvTime - SendTime) / 2),
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
            TotalDiffMS / TotalCountBigger0
    end.

calc_average_diff_ms([], TotalDiffMS, TotalCount, _OffsetMS, _AverageTransportDelay, _LogFile) ->
    {TotalDiffMS, TotalCount};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount, OffsetMS, AverageTransportDelay, LogFile) ->
    case messagehelper:get_station_type(CurrentMessage) of
        "A" ->
            TmpDiffMS = calc_diff_ms(CurrentMessage, OffsetMS),

            NewTotalDiffMS = TotalDiffMS + (TmpDiffMS - AverageTransportDelay),
            NewTotalCount = TotalCount + 1,

            logge_status("Diff (~p)", [TmpDiffMS], LogFile),
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, NewTotalCount, OffsetMS, AverageTransportDelay, LogFile);
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, OffsetMS, AverageTransportDelay, LogFile)
    end.

calc_diff_ms(Message, OffsetMS) ->
    SendTime = messagehelper:get_sendtime(Message),
    RecvTime = messagehelper:get_receivedtime(Message) + OffsetMS,
    TmpDiffMS = RecvTime - SendTime,
    TmpDiffMS.

calc_new_transport_delay_average(TransportTupel) ->
    {_StationName, TransportDelayTotal, TransportCount} = TransportTupel,
    case TransportCount of
        0 -> AverageTransportDelay = 0;
        _ -> AverageTransportDelay = round(TransportDelayTotal / TransportCount)
    end,
    AverageTransportDelay.

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