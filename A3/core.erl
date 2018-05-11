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

start(StationType, StationName, LogFile) ->
    start(StationType, StationName, LogFile, ?CLOCKOFFSETMS).

start(StationType, StationName, LogFile, ClockOffsetMS) ->
    logge_status("Startet as ~p", [StationName], LogFile),
    StationNumberString = lists:sublist(StationName, 9,2),

    RecvPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), LogFile),
    PayloadServerPid = payloadserver:start(node(), StationNumberString,LogFile),

    entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile).

entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile) ->
    receive
        newframe ->
            logge_status("New Frame Started", LogFile),
            {Messages, _StationWasInvolved} = listen_to_frame(RecvPid),
            ClockPid ! {adjust, Messages},
            SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName),
            logge_status("Selected SlotNumber ~p", [SlotNumber], LogFile),
            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile);
        Any -> 
            logge_status("Got (Entry): ~p", [Any], LogFile),
            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile)
    end.

send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile) ->
    receive
        newframe ->
            logge_status("New Frame Started with SlotNumber ~p", [SlotNumber], LogFile),
            ThisPid = self(),
            spawn(fun() -> 
                    MessageWasSend = prepare_and_send_message(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile),
                    ThisPid ! {messagewassend, MessageWasSend}
                end),
            {Messages, StationWasInvolved} = listen_to_frame(RecvPid),
            ClockPid ! {adjust, Messages},

            receive
                {messagewassend, MessageWasSend} -> 
                    logge_status("Send_Loop end with ~p (Involved) ~p (Send)",[StationWasInvolved, MessageWasSend], LogFile),
                    case (StationWasInvolved or not(MessageWasSend)) of
                        true ->
                            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile);
                        false ->
                            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile)
                    end
            end;
        Any -> 
            logge_status("Got (Send): ~p", [Any], LogFile),
            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile)
    end.

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


prepare_and_send_message(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid, LogFile) ->
    SendtimeMS = notify_when_preperation_and_send_due(ClockPid, SlotNumber, LogFile),
    receive
        preperation ->
            %logge_status("preperation", LogFile),
            IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
            receive
                send ->
                    ClockPid ! {getcurrenttime, self()},
                    %logge_status("send", LogFile),
                    receive
                        {currenttime, CurrentTime} ->
                            logge_status("~p (Seti) ~p (Now)", [SendtimeMS, CurrentTime], LogFile),
                            DiffTime = SendtimeMS - CurrentTime,
                            case DiffTime of
                                DiffTime when DiffTime > 40 -> 
                                    logge_status("SendTime in the future: ~p", [DiffTime], LogFile),
                                    timer:sleep(DiffTime - 40), %So he wakes up in the beginning of the slot
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS, LogFile),
                                    MessageWasSend = true;
                                DiffTime when DiffTime < -40 -> 
                                    logge_status("SendTime in the past: ~p", [DiffTime], LogFile),
                                    MessageWasSend = false; 
                                DiffTime -> 
                                    logge_status("SendTime is now: ~p", [DiffTime], LogFile),
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid, SendtimeMS,LogFile),
                                    MessageWasSend = true
                            end
                        after timer:seconds(1) ->
                            logge_status("Timeout resultdifftime", LogFile),
                            MessageWasSend = false
                    end
                after timer:seconds(1) ->
                    logge_status("Timeout send", LogFile),
                    MessageWasSend = false
            end
        after timer:seconds(1) ->
            logge_status("Timeout preperation", LogFile),
            MessageWasSend = false
    end,
    MessageWasSend.

send_message(IncompleteMessage, PayloadServerPid, SendPid, SendTime, _LogFile) ->
    PayloadServerPid ! {self(), getNextPayload},
    %logge_status("Warte auf Payload", LogFile),
    receive
        {payload, Payload} ->
            %logge_status("payload", LogFile),
            Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
            SendPid ! {send, Message}
    end.

notify_when_preperation_and_send_due(ClockPid, SlotNumber, _LogFile) ->
    ClockPid ! {calcslotbeginn, SlotNumber, self()},
    receive
        {resultslotbeginn, SendtimeMS} ->
            %logge_status("resultslotbeginn", LogFile),
            ClockPid ! {alarm, preperation, SendtimeMS - ?MESSAGEPREPERATIONTIMEMS, self()},
            ClockPid ! {alarm, send, SendtimeMS, self()}
    end,
    SendtimeMS.

%------------------------------------------
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p Core ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).