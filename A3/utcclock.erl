-module(utcclock).

-export([
    start/2,

    adjust/2,

    check_frame/4,
    new_frame_started/2,
    get_current_time/2,
    calc_slot_beginn_this_frame_time/2,
    set_alarm/3,

    get_8_byte_utc_binary/1
]).

-define(FRAMECHECKCYCLEMS, 10).
-define(FRAMELENGTHMS, 1000).
-define(SLOTLENGTHMS, 40).

start(OffsetMS, CorePid) ->
    Starttime = vsutil:getUTC(),
    logge_status(io_lib:format("Starttime: ~p\n", [Starttime])),
    FramecheckCycleMS = ?FRAMECHECKCYCLEMS,
    CurrentFrameNumber = 0,
    spawn(fun() -> loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid) end).

% --------------------------------------------------

loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid) ->
    timer:send_after(FramecheckCycleMS, self(), checkframe),
    receive
        {adjust, Messages} ->
            NewOffsetMS = adjust(OffsetMS, Messages),
            loop(Starttime, NewOffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid);
        checkframe ->
            NewCurrentFrameNumber = check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid),
            loop(Starttime, OffsetMS, FramecheckCycleMS, NewCurrentFrameNumber, CorePid);
        {calcslotbeginn, SlotNumber, SenderPid} ->
            SendtimeMS = calc_slot_beginn_this_frame_time(CurrentFrameNumber, SlotNumber),
            SenderPid ! {resultslotbeginn, SendtimeMS};
        {alarm, AlarmMessage, TimeWhenItsDue, SenderPid} ->
            TimeTillItsDue = TimeWhenItsDue - get_current_time(Starttime, OffsetMS),
            set_alarm(AlarmMessage, TimeTillItsDue, SenderPid),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid);
        {calcdifftime, SlotBeginnInFrame, SenderPid} ->
            CurrentTime = get_current_time(Starttime, OffsetMS),
            calc_diff_time(CurrentTime, SlotBeginnInFrame, SenderPid),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid);

        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid);
        {getcurrenttime, SenderPid} ->
            SenderPid ! get_current_time(Starttime, OffsetMS),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid);
        Any -> 
            io:fwrite("Got: ~p", [Any]),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CurrentFrameNumber, CorePid)
    end.

adjust(OffsetMS, Messages) ->
    AverageDiffMS = calc_average_diff_ms(Messages),
    NewOffsetMS = OffsetMS + AverageDiffMS,
    round(NewOffsetMS).

calc_average_diff_ms(Messages) ->
    {TotalDiffMS, TotalCount} = calc_average_diff_ms(Messages, 0, 0),
    case TotalCount of
        0 ->
            0;
        TotalCountBigger0 ->
            TotalDiffMS / TotalCountBigger0
    end.


calc_average_diff_ms([], TotalDiffMS, TotalCount) ->
    {TotalDiffMS, TotalCount};
calc_average_diff_ms([CurrentMessage | RestMessages], TotalDiffMS, TotalCount) ->
    case messagehelper:getStationType(CurrentMessage) of
        "A" ->
            SendTime = messagehelper:getSendTime(CurrentMessage),
            RecvTime = messagehelper:getReceivedTime(CurrentMessage),
            NewTotalDiffMS = TotalDiffMS + RecvTime - SendTime,
            calc_average_diff_ms(RestMessages, NewTotalDiffMS, TotalCount + 1);
        _Any ->
            calc_average_diff_ms(RestMessages, TotalDiffMS, TotalCount)
    end.

check_frame(Starttime, OffsetMS, CurrentFrameNumber, CorePid) ->   
    CurrentTime = get_current_time(Starttime, OffsetMS),
    case new_frame_started(CurrentTime, CurrentFrameNumber) of
        true ->
            CorePid ! newframe,
            logge_status(io_lib:format("New Frame at: ~p", [CurrentTime])),
            CurrentFrameNumber + 1;
        false ->
            logge_status(io_lib:format("No New Frame at: ~p", [CurrentTime])),
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

calc_diff_time(CurrentTime, SlotBeginnInFrame, SenderPid) ->
    DiffTime = CurrentTime - SlotBeginnInFrame,
    SenderPid ! {resultdifftime, DiffTime}.


get_8_byte_utc_binary(ErlangTS) ->
    TSAsUTC = vsutil:now2UTC(ErlangTS),
    Tmp = binary:encode_unsigned(TSAsUTC, big),
    <<0,0, Tmp/binary>>.

% --------------------------------------------------

logge_status(Inhalt) ->
    LOG_DATEI_NAME = "clock.log",
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    %io:fwrite(LogNachricht),
    util:logging(LOG_DATEI_NAME, LogNachricht).