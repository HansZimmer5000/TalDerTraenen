-module(core).

-export([
    start/3,
    start/4,

    listen_to_frame/1,
    listen_to_slot/1,


    notify_when_preperation_and_send_due/2
]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).

start(StationType, StationName, LogFile) ->
    start(StationType, StationName, LogFile, ?CLOCKOFFSETMS).

start(StationType, StationName, LogFile, ClockOffsetMS) ->
    logge_status("Startet", LogFile),

    RecvPid = receiver:start(self(), StationName, LogFile),
    SendPid = sender:start(LogFile),
    ClockPid = utcclock:start(ClockOffsetMS, self(), LogFile),
    PayloadServerPid = payloadserver:start(LogFile),

    entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile).

entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile) ->
    receive
        newframe ->
            logge_status("New Frame Started", LogFile),
            {Messages, _StationWasInvolved} = listen_to_frame(RecvPid),
            ClockPid ! {adjust, Messages},
            SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName),
            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile);
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile)
    end.

send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile) ->
    receive
        newframe ->
            ThisPid = self(),
            spawn(fun() -> 
                    MessageWasSend = prepare_and_send_message(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid),
                    ThisPid ! {messagewassend, MessageWasSend}
                end),
            {Messages, StationWasInvolved} = listen_to_frame(RecvPid),
            ClockPid ! {adjust, Messages},
            
            receive
                {messagewassend, MessageWasSend} -> 
                    case (StationWasInvolved or not(MessageWasSend)) of
                        true ->
                            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid, LogFile);
                        false ->
                            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber, LogFile)
                    end
            end;
        Any -> 
            logge_status("Got: ~p", [Any], LogFile),
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


prepare_and_send_message(SendPid, SlotNumber, StationType, ClockPid, PayloadServerPid) ->
    SendtimeMS = notify_when_preperation_and_send_due(ClockPid, SlotNumber),
    receive
        preperation ->
            IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
            receive
                send ->
                    ClockPid ! {calcdifftime, SendtimeMS, self()},
                    receive
                        {resultdifftime, DiffTime} ->
                            case DiffTime of
                                DiffTime when DiffTime < 0 -> 
                                    %SendTime is in the future
                                    timer:sleep(DiffTime),
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid),
                                    MessageWasSend = true;
                                DiffTime when DiffTime > 40 -> 
                                    %SendTime is in the past
                                    %TODO: !Wechsel zur Einstiegsphase
                                    MessageWasSend = false; 
                                DiffTime -> 
                                    %SendTime is now
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid),
                                    MessageWasSend = true
                            end
                    end
            end
    end,
    MessageWasSend.

send_message(IncompleteMessage, PayloadServerPid, SendPid) ->
    SendTime = vsutil:getUTC(),
    PayloadServerPid ! {self(), getNextPayload},
    receive
        {payload, Payload} ->
            Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload),
            SendPid ! {send, Message}
    end.

notify_when_preperation_and_send_due(ClockPid, SlotNumber) ->
    ClockPid ! {calcslotbeginn, SlotNumber, self()},
    receive
        {resultslotbeginn, SendtimeMS} ->
            ClockPid ! {alarm, preperation, SendtimeMS - ?MESSAGEPREPERATIONTIMEMS},
            ClockPid ! {alarm, send, SendtimeMS}
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