-module(core).

-export([start/0]).

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
            %Einstiegsphase ODER Sendephase
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
            %todo: Switch to Einstiegsphase if in Sendephase and StationWasInvolved = true
            {ConvertedSlotMessages, StationWasInvolved}
    end.


send_message(SendPid, SlotNumber, StationType,ClockPid, PayloadServerPid) ->
    inform_preperation(ClockPid, SlotNumber),
    inform_send(ClockPid, SlotNumber),
    receive
        preperation ->
            IncompleteMessage = messagehelper:createIncompleteMessage(StationType, SlotNumber),
            receive
                send ->
                    %TODO: Check clock if sendtime still in future (wait) or now.
                    SendTime = erlange:timestamp(),
                    PayloadServerPid ! {self(), getNextPayload},
                    receive
                        {payload, Payload} ->
                            Message = messagehelper:prepareIncompleteMessageForSending(IncompleteMessage, SendTime, Payload),
                            SendPid ! {send, Message}
                    end
            end
    end.

inform_preperation(ClockPid, SlotNumber) ->
    ClockPid ! {calcsendtime, SlotNumber},
    receive
        {sendtime, SendtimeMS} ->
            ClockPid ! {alarm, preperation, SendtimeMS - ?MESSAGEPREPERATIONTIMEMS}
    end.

inform_send(ClockPid, SlotNumber) ->
    ClockPid ! {calcsendtime, SlotNumber},
    receive
        {sendtime, SendtimeMS} ->
            ClockPid ! {alarm, send, SendtimeMS}
    end.

