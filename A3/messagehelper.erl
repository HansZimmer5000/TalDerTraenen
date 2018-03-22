-module(messagehelper).


-export([
    convertMessagesFromByte/1,
    convertMessagesFromByte/2,

    prepareForSending/2,
    addSendTime/2,
    convertMessageToByte/1
]).

-define(SLOTNUMBERPOS, 25).
-define(SENDTIMELENGTH, 8).

%Nachrichtenaufbau:
%    Gesamt 34 Byte // TTL = 1!
%    - Byte 0        A oder B    Stationsklasse
%    - Byte 1-24     -team-4711- Nutzdaten
%    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
%    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian

convertMessagesFromByte(MessagesInByte) -> 
    convertMessagesFromByte(MessagesInByte, []).

convertMessagesFromByte([], ConvertedMessages) ->
    ConvertedMessages;
convertMessagesFromByte(MessagesInByte, ConvertedMessages) ->
    [HeadMessage | RestMessages] = MessagesInByte,
    NewConvertedMessage = convertMessageFromByte(HeadMessage),
    NewConvertedMessages = [NewConvertedMessage | ConvertedMessages],
    convertMessagesFromByte(RestMessages, NewConvertedMessages).

convertMessageFromByte(MessageInByte) ->
    StationTypeAndPayload = binary:bin_to_list(MessageInByte, ?SLOTNUMBERPOS - 1),
    Slotnumber = binary:at(MessageInByte, ?SLOTNUMBERPOS),
    SendTime = binary:bin_to_list(MessageInByte, ?SLOTNUMBERPOS + 1, ?SENDTIMELENGTH),
    ConvertedMessage = lists:concat([
        StationTypeAndPayload, 
        Slotnumber,
        SendTime
    ]),
    ConvertedMessage.

prepareForSending(Message, SendTime) ->
    CompleteMessage = addSendTime(Message, SendTime),
    CompleteMessageAsByte = convertMessageToByte(CompleteMessage),
    CompleteMessageAsByte.

addSendTime(Message, SendTime) ->
    NewMessage = lists:concat([Message, SendTime]),
    NewMessage.

convertMessageToByte(Message) ->
    SlotnumberLength = length(Message) - 33,

    StationTypeAndPayload = lists:sublist(Message, ?SLOTNUMBERPOS),
    SlotnumberString = lists:sublist(
                            Message, 
                            ?SLOTNUMBERPOS + 1, 
                            SlotnumberLength),
    {Slotnumber, []} = string:to_integer(SlotnumberString),
    SendTime = lists:sublist(Message, length(Message) - ?SENDTIMELENGTH + 1, ?SENDTIMELENGTH + 1),
    ConvertedMessage = binary:list_to_bin([
        StationTypeAndPayload, 
        Slotnumber,
        SendTime
    ]),
    %io:lib:format("~w", [String]).
    io:fwrite("~p,~p,~p",[StationTypeAndPayload, Slotnumber, SendTime]),
    ConvertedMessage.
