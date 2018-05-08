-module(utcclock).

-export([
    start/2,

    adjust/2,

    get_8_byte_utc_binary/1
]).

-define(FRAMECHECKCYCLEMS, 10).

start(OffsetMS, CorePid) ->
    spawn(fun() -> loop(OffsetMS, CorePid) end).

% --------------------------------------------------

loop(OffsetMS, CorePid) ->
    timer:send_after(?FRAMECHECKCYCLEMS, self(), checkframe),
    receive
        {adjust, Messages} ->
            NewOffsetMS = adjust(OffsetMS, Messages),
            loop(NewOffsetMS, CorePid);
        checkframe ->
            check_frame(OffsetMS, CorePid),
            loop(OffsetMS, CorePid);
        {getcurrentoffsetms, Pid} ->
            %TODO: For Testing only?
            Pid ! OffsetMS,
            loop(OffsetMS, CorePid);
        Any -> 
            io:fwrite("Got: ~p", [Any]),
            loop(OffsetMS, CorePid)
    end.

adjust(OffsetMS, Messages) ->
    AverageDiffMS = calc_average_diff_ms(Messages),
    NewOffsetMS = OffsetMS + AverageDiffMS,
    NewOffsetMS.

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

check_frame(OffsetMS, CorePid) ->
    case new_frame_started(OffsetMS) of
        true ->
            CorePid ! newframe;
        false ->
            donothing
    end.

new_frame_started(OffsetMS) ->
    false.

get_8_byte_utc_binary(ErlangTS) ->
    TSAsUTC = vsutil:now2UTC(ErlangTS),
    Tmp = binary:encode_unsigned(TSAsUTC, big),
    <<0,0, Tmp/binary>>.

% --------------------------------------------------