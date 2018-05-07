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

    RecvPid ! listentoslot,
    SendPid ! {send, "hallo welt"},

    receive_loop().

receive_loop() ->
    receive
        Any -> io:fwrite("Core Got: ~p", [Any])
    end,
    receive_loop().
    
listen_to_slot(RecvPid, ClockPid) ->
    RecvPid ! listentoslot,
    receive
        {slotmessages, SlotMessages, ReceivedTimes} ->
            case collision_happend(SlotMessages) of
                false ->
                    ConvertedMessages = messagehelper:convertReceivedMessagesFromByte(SlotMessages, ReceivedTimes),
                    ClockPid ! {adjust, ConvertedMessages};
                true ->
                    donothing
            end
    end.

collision_happend(SlotMessages) ->
    case length(SlotMessages) of
        1 -> false;
        Any -> true
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

