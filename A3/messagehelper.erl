-module(messagehelper).


-export([
    convertReceivedMessagesFromByte/1,
    convertMessageFromByte/1,

    createIncompleteMessage/3,

    prepareIncompleteMessageForSending/2,
    addSendTime/2,
    convertMessageToByte/1,

    getStationType/1,

    getStationName/1,

    getSlotNumber/1
]).

-define(SLOTNUMBERPOS, 26). %Because String as List starts at 1 instead of 0.

%Nachrichtenaufbau:
%    Gesamt 34 Byte // TTL = 1!
%    - Byte 0        A oder B    Stationsklasse
%    - Byte 1-24     -team-0000- Nutzdaten
%    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
%    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian

convertReceivedMessagesFromByte(MessagesInByte) ->
    convertReceivedMessagesFromByte(MessagesInByte, []).

convertReceivedMessagesFromByte([], ConvertedMessages) ->
    ConvertedMessages;
convertReceivedMessagesFromByte(MessagesInByte, ConvertedMessages) ->
    [FirstMessageInByte | RestMessagesInByte] = MessagesInByte,
    ConvertedMessage = convertMessageFromByte(FirstMessageInByte),
    NewConvertedMessages = [ConvertedMessage | ConvertedMessages],
    convertReceivedMessagesFromByte(RestMessagesInByte, NewConvertedMessages).


convertMessageFromByte(MessageInByte) ->
    MessageInByteList = binary:bin_to_list(MessageInByte),
    SlotNumberLength = length(MessageInByteList) - 33,

    StationType = lists:sublist(MessageInByteList, 1, 1),
    Payload = lists:sublist(MessageInByteList, 2, 24),
    [SlotNumberString] = lists:sublist(MessageInByteList, ?SLOTNUMBERPOS, SlotNumberLength),
    SlotNumber = io_lib:format("~p", [SlotNumberString]),
    SendTime = lists:sublist(MessageInByteList, ?SLOTNUMBERPOS + SlotNumberLength, 8),

    ConvertedMessage = lists:append([StationType, Payload, SlotNumber, SendTime]),
    ConvertedMessage.

createIncompleteMessage(StationType, StationName, SlotNumber) ->
    Payload = StationName, % ++ AdditionalText, -> Vessel3 Connection needed!
    [SlotNumberString] = io_lib:format("~w", [SlotNumber]),
    (StationType ++ Payload) ++ SlotNumberString.

prepareIncompleteMessageForSending(IncompleteMessage, SendTime) ->
    CompleteMessage = addSendTime(IncompleteMessage, SendTime),
    CompleteMessage.

addSendTime(IncompleteMessage, SendTime) ->
    CompleteMessage = lists:concat([IncompleteMessage, SendTime]),
    CompleteMessage.

convertMessageToByte(Message) ->
    SlotNumberLength = length(Message) - 33,
    StationTypeAndPayload = lists:sublist(Message, 1, 25),
    {SlotNumber, []} = string:to_integer(lists:sublist(Message, 26, SlotNumberLength)),
    SendTime = lists:sublist(Message, 26 + SlotNumberLength, 8),

    binary:list_to_bin(lists:append([StationTypeAndPayload, [SlotNumber], SendTime])).

getStationType(Message) ->
    [FirstLetterAsAscii | _] = Message,
    [FirstLetterAsAscii]. %Converts Ascii to String

getStationName(Message) ->
    lists:sublist(Message, 2, 11).

getSlotNumber(Message) ->
    SlotNumberLength = length(Message) - 33,
    SlotNumberString = lists:sublist(Message, ?SLOTNUMBERPOS, SlotNumberLength),
    io:fwrite("~p = length, ~p = string", [SlotNumberLength, SlotNumberString]),
    {SlotNumber, []} = string:to_integer(SlotNumberString),
    SlotNumber.