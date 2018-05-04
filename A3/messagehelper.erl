-module(messagehelper).


-export([
    convertReceivedMessagesFromByte/2,
    convertMessageFromByte/2,

    createIncompleteMessage/2,

    prepareIncompleteMessageForSending/3,
    setSendTimeAndPayload/3,
    convertMessageToByte/1,

    getStationType/1,
    getStationName/1,
    getSlotNumber/1,
    getSendTime/1,
    getReceivedTime/1
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
    {StationType,Payload,SlotNumber,SendTime} = vsutil:message_to_string(MessageInByte),

    StationName = lists:sublist(Payload, 1, 11),
    ExtraPayload = lists:sublist(Payload, 12, 13),

    {{StationType, StationName, ExtraPayload, SlotNumber, SendTime}, ReceivedTime}.

createIncompleteMessage(StationType, SlotNumber) ->
    {{StationType, empty, empty, SlotNumber, empty}, empty}.

prepareIncompleteMessageForSending(IncompleteMessage, SendTime, Payload) ->
    CompleteMessage = setSendTimeAndPayload(IncompleteMessage, SendTime, Payload),
    convertMessageToByte(CompleteMessage).

setSendTimeAndPayload(IncompleteMessage, NewSendTime, Payload) ->
    {{StationType, empty, empty, SlotNumber, empty}, ReceivedTime} = IncompleteMessage,

    StationName = lists:sublist(Payload, 1, 11),
    ExtraPayload = lists:sublist(Payload, 12, 13),

    {{StationType, StationName, ExtraPayload, SlotNumber, NewSendTime}, ReceivedTime}.

convertMessageToByte(Message) ->
    {{StationType, StationName, ExtraPayload, SlotNumber, NewSendTime}, _} = Message,

    BinStation = vsutil:createBinaryS(StationType),
    BinData = vsutil:createBinaryD(StationName ++ ExtraPayload),
    BinNextSlot = vsutil:createBinaryNS(SlotNumber),
    BinTime = vsutil:createBinaryT(NewSendTime),

    vsutil:concatBinary(BinStation,BinData,BinNextSlot,BinTime).

% --------------------------------------------------

getStationType(Message) ->
    {{StationType, _, _, _, _}, _} = Message,
    StationType.

getStationName(Message) ->
    {{_, StationName, _, _, _}, _} = Message,
    StationName.

getSlotNumber(Message) ->
    {{_, _, _, SlotNumber, _}, _} = Message,
    SlotNumber.

getSendTime(Message) ->
    {{_, _, _, _, SendTime}, _} = Message,
    SendTime.

getReceivedTime(Message) ->
    {{_, _, _, _, _}, ReceivedTime} = Message,
    ReceivedTime.