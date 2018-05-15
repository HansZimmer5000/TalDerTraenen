-module(core).

-export([
    start/4,
    start/7,

    frame_loop/7,
    listen_to_frame/3,
    listen_to_slot/1,

    start_sending_process/8,
    notify_when_preperation_and_send_due/4
]).

-define(CLOCKOFFSETMS, 0).

start(StationType, StationName, ClockOffsetMS, LogFile) ->
    start(StationType, StationName, ClockOffsetMS, empty, empty, empty, LogFile).

start(StationType, StationName, ClockOffsetMS, InterfaceNameAtom, McastAddressAtom, ReceivePortAtom, LogFile) ->
    ReceivePort = erlang:list_to_integer(atom_to_list(ReceivePortAtom)),
    CorePid = self(),
    Pids = start_other_components(StationName, CorePid, InterfaceNameAtom, McastAddressAtom, ReceivePort, ClockOffsetMS, LogFile),
    frame_loop(StationName, StationType, 0, 0, false, Pids, LogFile).

start_other_components(StationName, CorePid, InterfaceNameAtom, McastAddressAtom, ReceivePort, ClockOffsetMS, LogFile) ->
    RecvPid = receiver:start(self(), StationName, InterfaceNameAtom, McastAddressAtom, ReceivePort,LogFile),
    SendPid = sender:start(McastAddressAtom, ReceivePort, LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    PayloadServerPid = payloadserver:start(LogFile),
    SlotFinderPid = slotfinder:start(CorePid, StationName, LogFile),
    {RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid}.

%-----------------------------------

frame_loop(StationName, StationType, FrameNumber, CurrentSlotNumber, InSendphase, Pids, LogFile) ->
    {RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid} = Pids,

    ClockPid ! {getcurrenttime, self()},
    receive 
        {currenttime, CurrentTime} ->  
            FrameStart = CurrentTime,
            logge_status("New Frame Started at ~p with Slot ~p --------", [CurrentTime, CurrentSlotNumber], LogFile),
            SlotFinderPid ! newframe,

            case InSendphase of
                false ->
                    continue;
                true ->
                    start_sending_process(SendPid, FrameStart, CurrentSlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile)
            end,
            {Messages, StationWasInvolved} = listen_to_frame_and_adjust_clock(RecvPid, ClockPid, SlotFinderPid, FrameStart, LogFile),
            logge_status("Received ~p Messages this Frame after ~p", [length(Messages), vsutil:getUTC() - FrameStart], LogFile),
            TimeElapsedInfRame = 1000 - vsutil:getUTC() - FrameStart,
            case TimeElapsedInfRame > 0 of
                true -> RestFrameTime = TimeElapsedInfRame;
                false -> RestFrameTime = 0
            end,
            case InSendphase of
                false ->
                    NextInSendphase = true,
                    NextSlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName);
                true -> 
                    receive
                        {messagewassend, MessageWasSend, NextSlotNumber} -> 
                            logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[StationWasInvolved, MessageWasSend], LogFile),
                            case (StationWasInvolved or not(MessageWasSend)) of
                                true ->
                                    NextInSendphase = false;
                                false ->
                                    NextInSendphase = true
                            end
                        after RestFrameTime ->
                            logge_status("Messagewassend was never received", LogFile),
                            NextInSendphase = false,
                            NextSlotNumber = 0
                    end
            end,
            logge_status("Frame Ended after ~p with NextInSendphase = ~p", [vsutil:getUTC() - FrameStart, NextInSendphase], LogFile),
            frame_loop(StationName, StationType, FrameNumber + 1, NextSlotNumber, NextInSendphase, Pids, LogFile)

        after timer:seconds(1) ->
            logge_status("Currenttime from Clock not within 1 second", LogFile),
            frame_loop(StationName, StationType, FrameNumber + 1, 0, false, Pids, LogFile)
    end.

%-----------------------------------

listen_to_frame_and_adjust_clock(RecvPid, ClockPid, SlotFinderPid, FrameStart, LogFile) ->
    {Messages, StationWasInvolved} = listen_to_frame(RecvPid, FrameStart, LogFile),
    ClockPid ! {adjust, Messages},
    SlotFinderPid ! {messageFromBC, Messages},
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

start_sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile) ->
    ThisPid = self(),
    spawn(fun() -> 
            {MessageWasSend, NextSlotNumber} = sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile),
            ThisPid ! {messagewassend, MessageWasSend, NextSlotNumber}
        end).

sending_process(SendPid, FrameStart, SlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile) ->
    SendtimeMS = notify_when_preperation_and_send_due(ClockPid, FrameStart, SlotNumber, LogFile),
    logge_status("SendTimeMS at ~p", [SendtimeMS], LogFile),
    {MessageWasSend, NextSlotNumber} = wait_for_prepare(StationType, SendtimeMS, ClockPid, SlotFinderPid, PayloadServerPid, SendPid, LogFile),
    {MessageWasSend, NextSlotNumber}.

notify_when_preperation_and_send_due(ClockPid, FrameStart, SlotNumber, _LogFile) ->
    ClockPid ! {calcslotmid, FrameStart, SlotNumber, self()},
    receive
        {resultslotmid, SendtimeMS} ->
            ClockPid ! {alarm, preperation, SendtimeMS - 20, self()},
            ClockPid ! {alarm, send, SendtimeMS - 10, self()}
    end,
    SendtimeMS.


wait_for_prepare(StationType, SendtimeMS, ClockPid, SlotFinderPid, PayloadServerPid, SendPid, LogFile) ->
    receive
        preperation ->
            SlotFinderPid ! {getFreeSlotNum, self()},
            receive 
                {slotnum, NextSlotNumber} -> 
                    NextSlotNumber,
                    logge_status("Got NextSlotnumber ~p", [NextSlotNumber], LogFile)
                after 20 -> 
                    NextSlotNumber = 0,
                    logge_status("Never received Slotnumber", LogFile),
                    exit(self(), kill) 
            end,
            IncompleteMessage = messagehelper:create_incomplete_message(StationType, NextSlotNumber),
            MessageWasSend = wait_for_send(SendtimeMS, IncompleteMessage, ClockPid, PayloadServerPid, SendPid, LogFile)
        after 980 ->
            logge_status("Timeout preperation", LogFile),
            MessageWasSend = false,
            NextSlotNumber = 0
    end,
    {MessageWasSend, NextSlotNumber}.

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

send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, LogFile) ->
    logge_status("Frage nach Payload", LogFile),
    Payload = request_payload(PayloadServerPid),
    logge_status("Sende Nachricht", LogFile),
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