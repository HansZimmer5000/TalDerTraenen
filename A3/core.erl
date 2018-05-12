-module(core).

-export([
    start/3,
    start/4,

    listen_to_frame/1,
    listen_to_slot/1,

    notify_when_preperation_and_send_due/3
]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).

%TODO: They are missing the slots way too often!!!

start(StationType, StationName, LogFile) ->
    start(StationType, StationName, LogFile, ?CLOCKOFFSETMS).

start(StationType, StationName, LogFile, ClockOffsetMS) ->
    StationNumberString = lists:sublist(StationName, 9,2),
    Pids = start_other_components(StationName, StationNumberString, ClockOffsetMS, LogFile),
    entry_loop(StationName, StationType, Pids, LogFile).

start_other_components(StationName, StationNumberString, ClockOffsetMS, LogFile) ->
    RecvPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    PayloadServerPid = payloadserver:start(node(), StationNumberString,LogFile),
    {RecvPid, SendPid, ClockPid, PayloadServerPid}.

%-----------------------------------

entry_loop(StationName, StationType, Pids, LogFile) ->
    {RecvPid, _SendPid, ClockPid, _PayloadServerPid} = Pids,
    receive
        newframe ->
            ClockPid ! {getcurrenttime, self()},
            receive 
                {currenttime, CurrentTime} ->  
                    logge_status("New Frame Started at ~p", [CurrentTime], LogFile)
            end,
            {Messages, _StationWasInvolved} = listen_to_frame_and_adjust_clock(RecvPid, ClockPid),
            SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName),
            logge_status("Received ~p Messages this Frame", [length(Messages)], LogFile),
            send_loop(StationName, StationType, Pids, SlotNumber, LogFile);
        Any -> 
            logge_status("Got Unkown (In Entryphase): ~p", [Any], LogFile),
            entry_loop(StationName, StationType, Pids, LogFile)
    end.

send_loop(StationName, StationType, Pids, SlotNumber, LogFile) ->
    {RecvPid, SendPid, ClockPid, PayloadServerPid} = Pids,
    receive
        newframe ->
            ClockPid ! {getcurrenttime, self()},
            receive 
                {currenttime, CurrentTime} ->  
                    logge_status("New Frame Started with SlotNumber ~p at ~p", [SlotNumber, CurrentTime], LogFile)
            end,
            start_sending_process(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile),
            {Messages, StationWasInvolved} = listen_to_frame_and_adjust_clock(RecvPid, ClockPid),
            logge_status("Received ~p Messages this Frame", [length(Messages)], LogFile),
            wait_for_messagewassend_and_handle_loop_end(
                StationWasInvolved, StationName, StationType, SlotNumber, Pids, LogFile);
        Any -> 
            logge_status("Got Unkown (In Sendphase): ~p", [Any], LogFile),
            send_loop(StationName, StationType, Pids, SlotNumber, LogFile)
    end.

%-----------------------------------

listen_to_frame_and_adjust_clock(RecvPid, ClockPid) ->
    {Messages, StationWasInvolved} = listen_to_frame(RecvPid),
    ClockPid ! {adjust, Messages},
    {Messages, StationWasInvolved}.

listen_to_frame(RecvPid) ->
    listen_to_frame(RecvPid, 25, [], false).

listen_to_frame(_RecvPid, 0, Messages, StationWasInvolved) ->
    {Messages, StationWasInvolved};
listen_to_frame(RecvPid, RestSlotCount, Messages, StationWasInvolved) ->
    {ReceivedMessages, ReceivedStationWasInvolved} = listen_to_slot(RecvPid),
    NewMessages = Messages ++ ReceivedMessages,
    NewStationWasInvolved = (StationWasInvolved or ReceivedStationWasInvolved),
    listen_to_frame(RecvPid, RestSlotCount - 1, NewMessages, NewStationWasInvolved).

listen_to_slot(RecvPid) ->
    RecvPid ! listentoslot,
    receive
        {slotmessages, ConvertedSlotMessages, StationWasInvolved} ->
            {ConvertedSlotMessages, StationWasInvolved}
    end.

start_sending_process(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile) ->
    ThisPid = self(),
    spawn(fun() -> 
            MessageWasSend = sending_process(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile),
            ThisPid ! {messagewassend, MessageWasSend}
        end).

sending_process(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile) ->
    SendtimeMS = notify_when_preperation_and_send_due(ClockPid, SlotNumber, LogFile),
    MessageWasSend = wait_for_prepare(StationType, SlotNumber, SendtimeMS, ClockPid, PayloadServerPid, SendPid, LogFile),
    MessageWasSend.

notify_when_preperation_and_send_due(ClockPid, SlotNumber, _LogFile) ->
    ClockPid ! {calcslotbeginn, SlotNumber, self()},
    receive
        {resultslotbeginn, SendtimeMS} ->
            ClockPid ! {alarm, preperation, SendtimeMS - ?MESSAGEPREPERATIONTIMEMS, self()},
            ClockPid ! {alarm, send, SendtimeMS, self()}
    end,
    SendtimeMS.


wait_for_prepare(StationType, SlotNumber, SendtimeMS, ClockPid, PayloadServerPid, SendPid, LogFile) ->
    receive
        preperation ->
            IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
            MessageWasSend = wait_for_send(SendtimeMS, IncompleteMessage, ClockPid, PayloadServerPid, SendPid, LogFile)
        after timer:seconds(1) ->
            logge_status("Timeout preperation", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

wait_for_send(SendtimeMS, IncompleteMessage, ClockPid, PayloadServerPid, SendPid, LogFile) ->
    receive
        send ->
            ClockPid ! {getcurrenttime, self()},
            MessageWasSend = wait_for_currenttime(SendtimeMS, IncompleteMessage, PayloadServerPid, SendPid, LogFile)
        after timer:seconds(1) ->
            logge_status("Timeout send", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

wait_for_currenttime(SendtimeMS, IncompleteMessage, PayloadServerPid, SendPid, LogFile) ->
    receive
        {currenttime, CurrentTime} ->
            MessageWasSend = check_sendtime_and_send(
                                SendtimeMS, CurrentTime, IncompleteMessage, PayloadServerPid, SendPid, LogFile)
        after timer:seconds(1) ->
            logge_status("Timeout resultdifftime", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

check_sendtime_and_send(SendtimeMS, CurrentTime, IncompleteMessage, PayloadServerPid, SendPid, LogFile) ->
    logge_status("~p (Seti) ~p (Now)", [SendtimeMS, CurrentTime], LogFile),
    DiffTime = SendtimeMS - CurrentTime,
    case DiffTime of
        DiffTime when DiffTime >= 40 -> 
            logge_status("SendTime in the future: ~p", [DiffTime], LogFile),
            timer:sleep(DiffTime - 40), %So he wakes up in the beginning of the slot
            send_message(IncompleteMessage, PayloadServerPid, SendPid, CurrentTime, LogFile),
            MessageWasSend = true;
        DiffTime when DiffTime =< -40 -> 
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

wait_for_messagewassend_and_handle_loop_end(StationWasInvolved, StationName, StationType, SlotNumber, Pids, LogFile) ->
    receive
        {messagewassend, MessageWasSend} -> 
            logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[StationWasInvolved, MessageWasSend], LogFile),
            case (StationWasInvolved or not(MessageWasSend)) of
                true ->
                    entry_loop(StationName, StationType, Pids, LogFile);
                false ->
                    send_loop(StationName, StationType, Pids, SlotNumber, LogFile)
            end
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