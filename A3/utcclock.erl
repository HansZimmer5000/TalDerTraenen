-module(utcclock).

-export([
    start/3,

    adjust/4,

    check_frame/4,
    new_frame_started/2,
    get_current_time/2,
    calc_slot_beginn_this_frame_time/2,
    set_alarm/4,
    calc_diff_time/2
]).

-define(FRAMECHECKCYCLEMS, 10).
-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(OffsetMS, CorePid, LogFile) ->
    Starttime = vsutil:getUTC(),
    logge_status("Starttime: ~p with Offset: ~p", [Starttime, OffsetMS], LogFile),
    CurrentFrameNumber = 0,
    ClockPid = spawn(fun() -> loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile) end),
    ClockPid.

% --------------------------------------------------

loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile) ->
    %TODO: wird öfter als CycleMS ausgeführt, da dies von der loop/6 Ausfuehrung abhaengt!
    timer:send_after(?FRAMECHECKCYCLEMS, self(), checkframe),
    receive
        {adjust, Messages} ->
            NewOffsetMS = adjust(Starttime, OffsetMS, Messages, LogFile),
            loop(Starttime, NewOffsetMS, CurrentFrameNumber, CorePid, LogFile);
        checkframe ->
            NewCurrentFrameNumber = check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid),
            loop(Starttime, OffsetMS, NewCurrentFrameNumber, CorePid, LogFile);
        {calcslotbeginn, SlotNumber, SenderPid} ->
            SendtimeMS = calc_slot_beginn_this_frame_time(CurrentFrameNumber, SlotNumber),
            SenderPid ! {resultslotbeginn, SendtimeMS},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile);
        {alarm, AlarmMessage, TimeWhenItsDue, SenderPid} ->
            TimeTillItsDue = TimeWhenItsDue - get_current_time(Starttime, OffsetMS),
            set_alarm(AlarmMessage, TimeTillItsDue, SenderPid, LogFile),
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile);
        {calcdifftime, SlotBeginnInFrame, SenderPid} ->
            CurrentTime = get_current_time(Starttime, OffsetMS),
            DiffTime = calc_diff_time(CurrentTime, SlotBeginnInFrame),
            SenderPid ! {resultdifftime, DiffTime},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile);
        {getcurrenttime, SenderPid} ->
            SenderPid ! {currenttime, get_current_time(Starttime, OffsetMS)},
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile);

        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile);
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            loop(Starttime, OffsetMS, CurrentFrameNumber, CorePid, LogFile)
    end.

% --------------------------------------------------

adjust(Starttime, OffsetMS, Messages, LogFile) ->
    AverageDiffMS = calc_average_diff_ms(Messages, Starttime, OffsetMS, LogFile),
    NewOffsetMS = round(OffsetMS - AverageDiffMS),
    logge_status("New Offset: ~p (Old: ~p)", [NewOffsetMS, OffsetMS], LogFile),
    NewOffsetMS.

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
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount, Starttime, OffsetMS, LogFile)
    end.

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