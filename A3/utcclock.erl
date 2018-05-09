-module(utcclock).

-export([
    start/2,

    adjust/2,

    check_frame/4,
    new_frame_started/2,
    get_current_time/2,

    get_8_byte_utc_binary/1
]).

-define(FRAMECHECKCYCLEMS, 10).

start(OffsetMS, CorePid) ->
    Starttime = vsutil:getUTC(),
    logge_status(io_lib:format("Starttime: ~p\n", [Starttime])),
    FramecheckCycleMS = ?FRAMECHECKCYCLEMS,
    FrameCount = 0,
    spawn(fun() -> loop(Starttime, OffsetMS, FramecheckCycleMS, FrameCount, CorePid) end).

% --------------------------------------------------

loop(Starttime, OffsetMS, FramecheckCycleMS, FrameCount, CorePid) ->
    timer:send_after(FramecheckCycleMS, self(), checkframe),
    receive
        {adjust, Messages} ->
            NewOffsetMS = adjust(OffsetMS, Messages),
            loop(Starttime, NewOffsetMS, FramecheckCycleMS, FrameCount, CorePid);
        checkframe ->
            NewFrameCount = check_frame(Starttime, OffsetMS, FrameCount, CorePid),
            loop(Starttime, OffsetMS, FramecheckCycleMS, NewFrameCount, CorePid);

        {getcurrentoffsetms, SenderPid} ->
            %For Testing only
            SenderPid ! OffsetMS,
            loop(Starttime, OffsetMS, FramecheckCycleMS, FrameCount, CorePid);
        {getcurrenttime, SenderPid} ->
            SenderPid ! get_current_time(Starttime, OffsetMS),
            loop(Starttime, OffsetMS, FramecheckCycleMS, FrameCount, CorePid);
        Any -> 
            io:fwrite("Got: ~p", [Any]),
            loop(Starttime, OffsetMS, FramecheckCycleMS, FrameCount, CorePid)
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

check_frame(Starttime, OffsetMS, FrameCount, CorePid) ->   
    %TODO: Mit zunehmender laufzeit erkennt er immer später erst wann der neue Frame beginnt!
    CurrentTime = get_current_time(Starttime, OffsetMS),
    case new_frame_started(CurrentTime, FrameCount) of
        true ->
            CorePid ! newframe,
            logge_status(io_lib:format("New Frame at: ~p", [CurrentTime])),
            FrameCount + 1;
        false ->
            logge_status(io_lib:format("No New Frame at: ~p", [CurrentTime])),
            FrameCount
    end.

new_frame_started(CurrentTime, FrameCount) ->
    %Ist die Aktuelle Zeit genau auf 0 Sekunden oder 0 Sekunden + FRAMECHECKCYCLE - 1?
    TimeElapsedInCurrentFrame = CurrentTime - (FrameCount * 1000),
    TimeElapsedInCurrentFrame >= 1000.

get_current_time(Starttime, OffsetMS) ->
    Result = vsutil:getUTC() - Starttime + OffsetMS,
    round(Result).

get_8_byte_utc_binary(ErlangTS) ->
    TSAsUTC = vsutil:now2UTC(ErlangTS),
    Tmp = binary:encode_unsigned(TSAsUTC, big),
    <<0,0, Tmp/binary>>.

% --------------------------------------------------

logge_status(Inhalt) ->
    LOG_DATEI_NAME = "clock.log",
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LOG_DATEI_NAME, LogNachricht).