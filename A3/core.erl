-module(core).

-export([
    start/3,
    start/4,

    listen_to_frame/3,
    listen_to_slot/1,

    notify_when_preperation_and_send_due/4
]).

-define(CLOCKOFFSETMS, 0).

%TODO: They are missing the slots way too often!!!

start(StationType, StationName, LogFile) ->
    start(StationType, StationName, LogFile, ?CLOCKOFFSETMS).

start(StationType, StationName, LogFile, ClockOffsetMS) ->
    StationNumberString = lists:sublist(StationName, 9,2),
    Pids = start_other_components(StationName, StationNumberString, ClockOffsetMS, LogFile),
    frame_loop(StationName, StationType, 0, empty, Pids, LogFile).

start_other_components(StationName, StationNumberString, ClockOffsetMS, LogFile) ->
    RecvPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    PayloadServerPid = payloadserver:start(node(), StationNumberString,LogFile),
    {RecvPid, SendPid, ClockPid, PayloadServerPid}.

%-----------------------------------

frame_loop(StationName, StationType, FrameNumber, SendingSlotNumber, Pids, LogFile) ->
    {RecvPid, SendPid, ClockPid, PayloadServerPid} = Pids,

    ClockPid ! {getcurrenttime, self()},
    receive 
        {currenttime, CurrentTime} ->  
            FrameStart = CurrentTime,
            logge_status("New Frame Started at ~p with Slot ~p", [CurrentTime, SendingSlotNumber], LogFile),

            case SendingSlotNumber of
                empty ->
                    continue;
                _ ->            
                    start_sending_process(SendPid, FrameStart, SendingSlotNumber, StationType, ClockPid, PayloadServerPid, LogFile)
            end,
            {Messages, StationWasInvolved} = listen_to_frame_and_adjust_clock(RecvPid, ClockPid, FrameStart, LogFile),
            logge_status("Received ~p Messages this Frame at ~p", [length(Messages), vsutil:getUTC() - FrameStart], LogFile),
            case SendingSlotNumber of
                empty ->
                    NextSlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName);
                _ -> 
                    receive
                        {messagewassend, MessageWasSend} -> 
                            logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[StationWasInvolved, MessageWasSend], LogFile),
                            case (StationWasInvolved or not(MessageWasSend)) of
                                true ->
                                    NextSlotNumber = empty;
                                false ->
                                    NextSlotNumber = SendingSlotNumber
                            end
                    end
            end,
            logge_status("Frame Ended at ~p", [vsutil:getUTC() - FrameStart], LogFile),
            frame_loop(StationName, StationType, FrameNumber + 1, NextSlotNumber, Pids, LogFile)
    end.

%-----------------------------------

listen_to_frame_and_adjust_clock(RecvPid, ClockPid, FrameStart, LogFile) ->
    {Messages, StationWasInvolved} = listen_to_frame(RecvPid, FrameStart, LogFile),
    ClockPid ! {adjust, Messages},
    {Messages, StationWasInvolved}.

listen_to_frame(RecvPid, FrameStart, LogFile) ->
    listen_to_frame(RecvPid, 25, [], false, FrameStart, LogFile).

listen_to_frame(_RecvPid, 0, Messages, StationWasInvolved, _FrameStart, _LogFile) ->
    {Messages, StationWasInvolved};
listen_to_frame(RecvPid, RestSlotCount, Messages, StationWasInvolved, FrameStart, LogFile) ->
    {ReceivedMessages, ReceivedStationWasInvolved} = listen_to_slot(RecvPid),
    %logge_status("Got SlotMessages at ~p", [vsutil:getUTC() - FrameStart], LogFile),
    NewMessages = Messages ++ ReceivedMessages,
    NewStationWasInvolved = (StationWasInvolved or ReceivedStationWasInvolved),
    listen_to_frame(RecvPid, RestSlotCount - 1, NewMessages, NewStationWasInvolved, FrameStart, LogFile).

listen_to_slot(RecvPid) ->
    RecvPid ! listentoslot,
    receive
        {slotmessages, ConvertedSlotMessages, StationWasInvolved} ->
            {ConvertedSlotMessages, StationWasInvolved}
    end.

start_sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile) ->
    ThisPid = self(),
    spawn(fun() -> 
            MessageWasSend = sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile),
            ThisPid ! {messagewassend, MessageWasSend}
        end).

sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile) ->
    SendtimeMS = notify_when_preperation_and_send_due(ClockPid, FrameStart, SlotNumber, LogFile),
    logge_status("SendTimeMS at ~p", [SendtimeMS], LogFile),
    MessageWasSend = wait_for_prepare(StationType, SlotNumber, SendtimeMS, ClockPid, PayloadServerPid, SendPid, LogFile),
    MessageWasSend.

notify_when_preperation_and_send_due(ClockPid, FrameStart, SlotNumber, _LogFile) ->
    ClockPid ! {calcslotmid, FrameStart, SlotNumber, self()},
    receive
        {resultslotmid, SendtimeMS} ->
            ClockPid ! {alarm, preperation, SendtimeMS - 20, self()},
            ClockPid ! {alarm, send, SendtimeMS - 10, self()}
    end,
    SendtimeMS.


wait_for_prepare(StationType, SlotNumber, SendtimeMS, ClockPid, PayloadServerPid, SendPid, LogFile) ->
    receive
        preperation ->
            IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
            MessageWasSend = wait_for_send(SendtimeMS, IncompleteMessage, ClockPid, PayloadServerPid, SendPid, LogFile)
        after 980 ->
            logge_status("Timeout preperation", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

wait_for_send(SendtimeMS, IncompleteMessage, ClockPid, PayloadServerPid, SendPid, LogFile) ->
    receive
        send ->
            ClockPid ! {getcurrenttime, self()},
            MessageWasSend = wait_for_currenttime(SendtimeMS, IncompleteMessage, PayloadServerPid, SendPid, LogFile)
        after 980 ->
            logge_status("Timeout send", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

wait_for_currenttime(SendtimeMS, IncompleteMessage, PayloadServerPid, SendPid, LogFile) ->
    receive
        {currenttime, CurrentTime} ->
            MessageWasSend = check_sendtime_and_send(
                                SendtimeMS, CurrentTime, IncompleteMessage, PayloadServerPid, SendPid, LogFile)
        after 980 ->
            logge_status("Timeout resultdifftime", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

check_sendtime_and_send(SendtimeMS, CurrentTime, IncompleteMessage, PayloadServerPid, SendPid, LogFile) ->
    logge_status("~p (Seti) ~p (Now)", [SendtimeMS, CurrentTime], LogFile),
    DiffTime = SendtimeMS - CurrentTime,
    case DiffTime of
        DiffTime when DiffTime > 0 -> 
            logge_status("SendTime in the future: ~p", [DiffTime], LogFile),
            timer:sleep(DiffTime), %So he wakes up in the beginning of the slot
            send_message(IncompleteMessage, PayloadServerPid, SendPid, CurrentTime, LogFile),
            MessageWasSend = true;
        DiffTime when DiffTime < 0 -> 
            logge_status("SendTime in the past: ~p", [DiffTime], LogFile),
            MessageWasSend = false; 
        DiffTime -> 
            logge_status("SendTime is now: ~p", [DiffTime], LogFile),
            send_message(IncompleteMessage, PayloadServerPid, SendPid, CurrentTime,LogFile),
            MessageWasSend = true
    end,
    MessageWasSend.

send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, _LogFile) ->
    Payload = request_payload(PayloadServerPid),
    Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
    SendPid ! {send, Message}.

request_payload(PayloadServerPid) ->
    PayloadServerPid ! {self(), getNextPayload},
    receive
        {payload, Payload} ->
            Payload
    end.

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p Core ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).