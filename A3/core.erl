-module(core).

-export([
    start/4,
    start/7,

    frame_loop/6,
    listen_to_slot/4,

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
    frame_loop(StationName, StationType, 0, false, Pids, LogFile).

start_other_components(StationName, CorePid, InterfaceNameAtom, McastAddressAtom, ReceivePort, ClockOffsetMS, LogFile) ->
    ClockPid = utcclock:start(ClockOffsetMS, self(), StationName, LogFile),
    RecvPid = receiver:start(self(), ClockPid, InterfaceNameAtom, McastAddressAtom, ReceivePort,LogFile),
    SendPid = sender:start(McastAddressAtom, ReceivePort, LogFile),
    PayloadServerPid = payloadserver:start(LogFile),
    SlotFinderPid = slotfinder:start(CorePid, StationName, LogFile),
    {RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid}.

%-----------------------------------

frame_loop(StationName, StationType, CurrentSlotNumber, InSendphase, Pids, LogFile) ->
    {_RecvPid, SendPid, ClockPid, SlotFinderPid, PayloadServerPid} = Pids,
    CorePid = self(),

    ClockPid ! {getcurrenttime, self()},
    receive 
        {currenttime, StationFrameStart} ->  
            MaschineFrameStart = vsutil:getUTC(),
            logge_status("New Frame Started at ~p with Slot ~p --------", [StationFrameStart, CurrentSlotNumber], LogFile),
            SlotFinderPid ! newframe,

            case InSendphase of
                false ->
                    continue;
                true ->
                    start_sending_process(SendPid, StationFrameStart, CurrentSlotNumber, StationType, ClockPid, SlotFinderPid, PayloadServerPid, LogFile)
            end,
            listen_to_slots_and_adjust_clock_and_slots(25, ClockPid, SlotFinderPid, CorePid, StationName, LogFile),
            TimeElapsedInFrame = 1000 - vsutil:getUTC() - MaschineFrameStart,
            case TimeElapsedInFrame > 0 of
                true -> RestFrameTime = TimeElapsedInFrame;
                false -> RestFrameTime = 0
            end,
            logge_status("RestFrameTime = ~p", [RestFrameTime], LogFile),
            case InSendphase of
                false ->
                    SlotFinderPid ! {getFreeSlotNum, self()},
                    receive 
                        {slotnum, NextSlotNumber} -> 
                            NextInSendphase = true
                        after RestFrameTime -> 
                            NextSlotNumber = 0,
                            NextInSendphase = false,
                            logge_status("Never received Slotnumber", LogFile)
                    end;
                true -> 
                    receive
                        {messagewassend, MessageWasSend, ReceivedNextSlotNumber} -> 
                            receive 
                                {stationwasinvolved, true} ->
                                    logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[true, MessageWasSend], LogFile),
                                    NextInSendphase = false,
                                    NextSlotNumber = 0
                                after RestFrameTime -> 
                                    logge_status("stationwasinvolved,true was never received", LogFile),
                                    case MessageWasSend of
                                        true ->
                                            NextInSendphase = true,
                                            NextSlotNumber = ReceivedNextSlotNumber;
                                        false ->
                                            NextInSendphase = false,
                                            NextSlotNumber = 0
                                    end
                            end
                        after RestFrameTime ->
                            logge_status("Messagewassend was never received", LogFile),
                            NextInSendphase = false,
                            NextSlotNumber = 0
                    end
            end,
            TimeTillEnd = 1000 - vsutil:getUTC() - MaschineFrameStart,
            case TimeTillEnd > 0 of
                true -> timer:sleep(TimeTillEnd -1); 
                false -> continue
            end,
            logge_status("Frame Ended after ~p with NextInSendphase = ~p", [vsutil:getUTC() - MaschineFrameStart, NextInSendphase], LogFile),
            frame_loop(StationName, StationType, NextSlotNumber, NextInSendphase, Pids, LogFile)

        after timer:seconds(1) ->
            logge_status("Currenttime from Clock not within 1 second", LogFile),
            frame_loop(StationName, StationType, 0, false, Pids, LogFile)
    end.

%-----------------------------------
listen_to_slots_and_adjust_clock_and_slots(0, _ClockPid, _SlotFinderPid, _CorePid, _StationName, _LogFile) ->
    done;
listen_to_slots_and_adjust_clock_and_slots(RestSlotCount, ClockPid, SlotFinderPid, CorePid, StationName, LogFile) ->
    {ReceivedMessages, ReceivedTimes} = listen_to_slot(40, [],[], LogFile),
    spawn(fun() -> 
            logge_status("Received ~p Messages this slot", [length(ReceivedMessages)], LogFile),
            case length(ReceivedMessages) of
                0 ->
                    CorePid ! {stationwasinvolved, false};
                _Any ->
                    ConvertedMessages = messagehelper:convert_received_messages_from_byte(ReceivedMessages, ReceivedTimes),
                    {CollisionHappend, StationWasInvolved} = receiver:collision_happend(ConvertedMessages, StationName, LogFile),
                    case CollisionHappend of
                        true -> 
                            CorePid ! {stationwasinvolved, StationWasInvolved};
                        false ->
                            ClockPid ! {adjust, ConvertedMessages},
                            SlotFinderPid ! {newmessages, ConvertedMessages},
                            CorePid ! {stationwasinvolved, StationWasInvolved}
                    end
            end
        end),
    %logge_status("Got SlotMessages at ~p", [vsutil:getUTC() - FrameStart], LogFile),
    listen_to_slots_and_adjust_clock_and_slots(RestSlotCount - 1, ClockPid, SlotFinderPid, CorePid, StationName, LogFile).

listen_to_slot(RestSlotTime, Messages, ReceivedTimes, _LogFile) when RestSlotTime =< 0 ->
    {Messages, ReceivedTimes};
listen_to_slot(RestSlotTime, Messages, ReceivedTimes, LogFile) ->
    StartTime = vsutil:getUTC(),
    %logge_status("Waiting for next receivedmessage", LogFile),
    receive
        {receivedmessage, Message, ReceivedTime} ->
            NewMessages = [Message | Messages],
            NewReceivedTimes = [ReceivedTime | ReceivedTimes],
            NewRestSlotTime = vsutil:getUTC() - StartTime,
            listen_to_slot(NewRestSlotTime, NewMessages, NewReceivedTimes, LogFile)
        after RestSlotTime ->  
            %logge_status("Got no receivedmessage this slot", LogFile),
            {Messages, ReceivedTimes}
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