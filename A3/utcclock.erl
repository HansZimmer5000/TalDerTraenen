-module(utcclock).

-export([
    start/2,

    adjust/2,

    check_frame/4,
    new_frame_started/3,

    get_8_byte_utc_binary/1
]).

-define(FRAMECHECKCYCLEMS, 10).

start(OffsetMS, CorePid) ->
    Starttime = vsutil:getUTC(),
    FramecheckCycleMS = ?FRAMECHECKCYCLEMS,
    spawn(fun() -> loop(Starttime, OffsetMS, FramecheckCycleMS, CorePid) end).

% --------------------------------------------------

loop(Starttime, OffsetMS, FramecheckCycleMS, CorePid) ->
    timer:send_after(FramecheckCycleMS, self(), checkframe),
    receive
        {adjust, Messages} ->
            NewOffsetMS = adjust(OffsetMS, Messages),
            loop(Starttime, NewOffsetMS, FramecheckCycleMS, CorePid);
        checkframe ->
            check_frame(Starttime, OffsetMS, FramecheckCycleMS, CorePid),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CorePid);
        {getcurrentoffsetms, SenderPid} ->
            SenderPid ! OffsetMS,
            loop(Starttime, OffsetMS, FramecheckCycleMS, CorePid);
        Any -> 
            io:fwrite("Got: ~p", [Any]),
            loop(Starttime, OffsetMS, FramecheckCycleMS, CorePid)
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

check_frame(Starttime, OffsetMS, FramecheckCycleMS, CorePid) ->
    case new_frame_started(Starttime, OffsetMS, FramecheckCycleMS) of
        true ->
            CorePid ! newframe;
        false ->
            donothing
    end.

new_frame_started(Starttime, OffsetMS, FramecheckCycleMS) ->
    %Ist die Aktuelle Zeit genau auf 0 Sekunden oder 0 Sekunden + FRAMECHECKCYCLE - 1?
    TimeElapsedInCurrentFrame = get_current_time(Starttime, OffsetMS) rem 1000,
    (TimeElapsedInCurrentFrame >= 0) and (TimeElapsedInCurrentFrame < (FramecheckCycleMS - 1)).

get_current_time(Starttime, OffsetMS) ->
    Result = vsutil:getUTC() - Starttime + OffsetMS,
    round(Result).

get_8_byte_utc_binary(ErlangTS) ->
    TSAsUTC = vsutil:now2UTC(ErlangTS),
    Tmp = binary:encode_unsigned(TSAsUTC, big),
    <<0,0, Tmp/binary>>.

% --------------------------------------------------