-module(core).

-export([
    start/1,

    listen_to_frame/1,
    listen_to_slot/1,


    notify_when_preperation_and_send_due/2
]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).

start([StationTypeAtom]) ->
    StationType = atom_to_list(StationTypeAtom),
    start(StationType, ?CLOCKOFFSETMS);
start(StationType) ->
    start(StationType, ?CLOCKOFFSETMS).

start(StationType, ClockOffsetMS) ->
    io:fwrite("start"),
    StationName = "-team-0602-",

    RecvPid = receiver:start(self(), StationName),
    SendPid = sender:start(),
    ClockPid = utcclock:start(ClockOffsetMS, self()),
    PayloadServerPid = payloadserver:start(),

    entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid).

entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid) ->
    receive
        newframe ->
            {Messages, _StationWasInvolved} = listen_to_frame(RecvPid),
            ClockPid ! {adjust, Messages},
            SlotNumber = slotfinder:find_slot_in_next_frame(Messages, StationName),
            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber);
        Any -> 
            io:fwrite("Core Got: ~p", [Any]),
            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid)
    end.

send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber) ->
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
                            entry_loop(StationName, StationType, RecvPid, SendPid, PayloadServerPid, ClockPid);
                        false ->
                            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber)
                    end
            end;
        Any -> 
            io:fwrite("Core Got: ~p", [Any]),
            send_loop(StationName, StationType, RecvPid, SendPid, ClockPid, PayloadServerPid, SlotNumber)
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

