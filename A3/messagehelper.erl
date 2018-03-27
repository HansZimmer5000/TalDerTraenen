-module(messagehelper).


-export([
    prepareIncompleteMessageForSending/2,
    addSendTime/2,

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

prepareIncompleteMessageForSending(IncompleteMessage, SendTime) ->
    CompleteMessage = addSendTime(IncompleteMessage, SendTime),
    CompleteMessage.

addSendTime(IncompleteMessage, SendTime) ->
    CompleteMessage = lists:concat([IncompleteMessage, SendTime]),
    CompleteMessage.

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