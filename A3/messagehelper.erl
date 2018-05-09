-module(messagehelper).


-export([
    convert_received_messages_from_byte/2,
    convert_message_from_byte/2,

    create_incomplete_message/2,

    prepare_incomplete_message_for_sending/3,
    set_sendtime_and_payload/3,
    convert_message_to_byte/1,

    get_station_type/1,
    get_station_name/1,
    get_slotnumber/1,
    get_sendtime/1,
    get_receivedtime/1
]).

-define(SLOTNUMBERPOS, 26). %Because String as List starts at 1 instead of 0.
-define(MESSAGE_AS_STRING_LENGTH, 39).

%Nachrichtenaufbau Binary:
%    Gesamt 34 Byte // TTL = 1!
%    - Byte 0        A oder B    Stationsklasse
%    - Byte 1-24     -team-0000- Nutzdaten
%    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
%    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian

%Nachrichtenaufbau Intern:
% {{StationType, StationName, ExtraPayload, SlotNumber, SendTime}, ReceivedTime}
%   (Always) StationType = String with "A" or "B"
%   (Always) StationName = String with "-team-0000-"
%   (Always) ExtraPayload = String with some letters of 13 letters length
%   (Always) SlotNumber = Number with 1 or 2 numbers (e.g. 4 or 25)
%   (When Added) SendTime = Number with 13 numbers (UTC with e.g. vsutil:getUTC())
%   (When Message was Received) RecievedTime = Same format as SendTime

convert_received_messages_from_byte(MessagesInByte, ReceivedTimes) ->
    convert_received_messages_from_byte(MessagesInByte, ReceivedTimes, []).

convert_received_messages_from_byte([], [], ConvertedMessages) ->
    ConvertedMessages;
convert_received_messages_from_byte(MessagesInByte, ReceivedTimes, ConvertedMessages) ->
    [FirstMessageInByte | RestMessagesInByte] = MessagesInByte,
    [FirstReceivedTime | RestReceivedTimes] = ReceivedTimes,
    ConvertedMessage = convert_message_from_byte(FirstMessageInByte, FirstReceivedTime),
    NewConvertedMessages = [ConvertedMessage | ConvertedMessages],
    convert_received_messages_from_byte(RestMessagesInByte, RestReceivedTimes, NewConvertedMessages).


convert_message_from_byte(MessageInByte, ReceivedTime) ->
    {StationType,Payload,SlotNumber,SendTime} = vsutil:message_to_string(MessageInByte),

    StationName = lists:sublist(Payload, 1, 10),
    ExtraPayload = lists:sublist(Payload, 11, 14),

    {{StationType, StationName, ExtraPayload, SlotNumber, SendTime}, ReceivedTime}.

create_incomplete_message(StationType, SlotNumber) ->
    {{StationType, empty, empty, SlotNumber, empty}, empty}.

prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, Payload) ->
    CompleteMessage = set_sendtime_and_payload(IncompleteMessage, SendTime, Payload),
    convert_message_to_byte(CompleteMessage).

set_sendtime_and_payload(IncompleteMessage, NewSendTime, Payload) ->
    {{StationType, empty, empty, SlotNumber, empty}, ReceivedTime} = IncompleteMessage,
    
    StationName = lists:sublist(Payload, 1, 10),
    ExtraPayload = lists:sublist(Payload, 11, 14),

    {{StationType, StationName, ExtraPayload, SlotNumber, NewSendTime}, ReceivedTime}.

convert_message_to_byte(Message) ->
    {{StationType, StationName, ExtraPayload, SlotNumber, NewSendTime}, _} = Message,

    BinStation = vsutil:createBinaryS(StationType),
    BinData = vsutil:createBinaryD(StationName ++ ExtraPayload),
    BinNextSlot = vsutil:createBinaryNS(SlotNumber),
    BinTime = vsutil:createBinaryT(NewSendTime),

    vsutil:concatBinary(BinStation,BinData,BinNextSlot,BinTime).

% --------------------------------------------------

get_station_type(Message) ->
    {{StationType, _, _, _, _}, _} = Message,
    StationType.

get_station_name(Message) ->
    {{_, StationName, _, _, _}, _} = Message,
    StationName.

get_slotnumber(Message) ->
    {{_, _, _, SlotNumber, _}, _} = Message,
    SlotNumber.

get_sendtime(Message) ->
    {{_, _, _, _, SendTime}, _} = Message,
    SendTime.

get_receivedtime(Message) ->
    {{_, _, _, _, _}, ReceivedTime} = Message,
    ReceivedTime.