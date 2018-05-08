-module(core).

-export([start/0]).

-define(CLOCKOFFSETMS, 0).
-define(MESSAGEPREPERATIONTIMEMS, 10).

start() ->
    start(?CLOCKOFFSETMS).

start(ClockOffsetMS) ->
    io:fwrite("start"),
    RecvPid = receiver:start(self()),
    SendPid = sender:start(),
    ClockPid = utcclock:start(ClockOffsetMS),
    _PayloadServerPid = payloadserver:start(),

    receive_loop(RecvPid, ClockPid).

receive_loop(RecvPid, ClockPid) ->
    receive
        newframe ->
            %Einstiegsphase
            %Sendephase
            receive_loop(RecvPid, ClockPid);
        Any -> 
            io:fwrite("Core Got: ~p", [Any]),
            receive_loop(RecvPid, ClockPid)
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

