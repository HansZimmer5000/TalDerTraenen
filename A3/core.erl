-module(core).

-export([
    start/0,

    listen_to_slot/1,

    notify_when_preperation_and_send_due/2
]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).

start() ->
    start(?CLOCKOFFSETMS).

start(ClockOffsetMS) ->
    io:fwrite("start"),
    StationName = "-team-0602-",

    RecvPid = receiver:start(self(), StationName),
    SendPid = sender:start(),
    ClockPid = utcclock:start(ClockOffsetMS, self()),
    _PayloadServerPid = payloadserver:start(),

    receive_loop(RecvPid, SendPid, ClockPid).

receive_loop(RecvPid, SendPid, ClockPid) ->
    receive
        newframe ->
            %Einstiegsphase UND Sendephase
            %io:fwrite("New Frame started: ~p--~p\n", [vsutil:now2string(erlang:timestamp()), vsutil:getUTC()]),
            receive_loop(RecvPid, SendPid, ClockPid);
        Any -> 
            io:fwrite("Core Got: ~p", [Any]),
            receive_loop(RecvPid, SendPid, ClockPid)
    end.

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
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid);
                                DiffTime when DiffTime > 40 -> 
                                    %SendTime is in the past
                                    %TODO: !Wechsel zur Einstiegsphase
                                    donothing; 
                                DiffTime -> 
                                    %SendTime is now
                                    send_message(IncompleteMessage, PayloadServerPid, SendPid)
                            end
                    end
            end
    end.

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

