-module(messagehelper).


-export([
    convertReceivedMessagesFromByte/2,
    convertMessageFromByte/2,

    createIncompleteMessage/3,

    prepareIncompleteMessageForSending/2,
    setSendTime/2,
    convertMessageToByte/1,

    getStationType/1,

    getStationName/1,

    getSlotNumber/1
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
% {{StationType, StationName, Payload, SlotNumber, SendTime}, ReceivedTime}
%   (Always) StationType = String with "A" or "B"
%   (Always) StationName = String with "-team-0000-"
%   (Always) Payload = String with some letters of 13 letters length
%   (Always) SlotNumber = Number with 1 or 2 numbers (e.g. 4 or 25)
%   (When Added) SendTime = Number with 13 numbers (UTC with e.g. vsutil:getUTC())
%   (When Message was Received) RecievedTime = Same format as SendTime

convertReceivedMessagesFromByte(MessagesInByte, ReceivedTimes) ->
    convertReceivedMessagesFromByte(MessagesInByte, ReceivedTimes, []).

convertReceivedMessagesFromByte([], [], ConvertedMessages) ->
    ConvertedMessages;
convertReceivedMessagesFromByte(MessagesInByte, ReceivedTimes, ConvertedMessages) ->
    [FirstMessageInByte | RestMessagesInByte] = MessagesInByte,
    [FirstReceivedTime | RestReceivedTimes] = ReceivedTimes,
    ConvertedMessage = convertMessageFromByte(FirstMessageInByte, FirstReceivedTime),
    NewConvertedMessages = [ConvertedMessage | ConvertedMessages],
    convertReceivedMessagesFromByte(RestMessagesInByte, RestReceivedTimes, NewConvertedMessages).


convertMessageFromByte(MessageInByte, ReceivedTime) ->
    MessageInByteList = binary:bin_to_list(MessageInByte),
    SlotNumberLength = length(MessageInByteList) - 33,

    StationType = lists:sublist(MessageInByteList, 1, 1),
    StationName = lists:sublist(MessageInByteList, 2, 11),
    Payload = lists:sublist(MessageInByteList, 13, 13),
    [SlotNumber] = lists:sublist(MessageInByteList, ?SLOTNUMBERPOS, SlotNumberLength),
    SendTimeList = lists:sublist(MessageInByteList, ?SLOTNUMBERPOS + SlotNumberLength, 8),
    SendTimeBinary = binary:list_to_bin(SendTimeList),
    SendTime = binary:decode_unsigned(SendTimeBinary, big),

    {{StationType, StationName, Payload, SlotNumber, SendTime}, ReceivedTime}.

createIncompleteMessage(StationType, StationName, SlotNumber) ->
    Payload = empty, % -> Vessel3 Connection needed!
    {{StationType, StationName, Payload, SlotNumber, empty}, empty}.

prepareIncompleteMessageForSending(IncompleteMessage, SendTime) ->
    CompleteMessage = setSendTime(IncompleteMessage, SendTime),
    convertMessageToByte(CompleteMessage).

setSendTime(IncompleteMessage, NewSendTime) ->
    {{StationType, StationName, Payload, SlotNumber, _SendTime}, ReceivedTime} = IncompleteMessage,
    {{StationType, StationName, Payload, SlotNumber, NewSendTime}, ReceivedTime}.

convertMessageToByte(Message) ->
    {{StationType, StationName, Payload, SlotNumber, SendTime}, _} = Message,
    StationTypeAndPayloadBinary = binary:list_to_bin((StationType ++ StationName) ++ Payload),
    TempSendTimeBinary = binary:encode_unsigned(SendTime, big),
    SendTimeBinary = <<0,0, TempSendTimeBinary/binary>>,

    <<StationTypeAndPayloadBinary/binary, SlotNumber, SendTimeBinary/binary>>.
    %binary:list_to_bin(lists:append([StationTypeAndPayload, [SlotNumber], SendTime])).

getStationType(Message) ->
    {{StationType, _, _, _, _}, _} = Message,
    StationType.

getStationName(Message) ->
    {{_, StationName, _, _, _}, _} = Message,
    StationName.

getSlotNumber(Message) ->
    {{_, _, _, SlotNumber, _}, _} = Message,
    SlotNumber.