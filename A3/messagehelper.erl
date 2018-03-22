-module(messagehelper).


-export([
    prepareIncompleteMessageForSending/2,
    addSendTime/2,

    getStationType/1,

    getStationName/1,

    getSlotNumber/1
]).

-define(SLOTNUMBERPOS, 25).
-define(SENDTIMELENGTH, 8).

%Nachrichtenaufbau:
%    Gesamt 34 Byte // TTL = 1!
%    - Byte 0        A oder B    Stationsklasse
%    - Byte 1-24     -team-4711- Nutzdaten
%    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
%    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian

prepareIncompleteMessageForSending(IncompleteMessage, SendTime) ->
    CompleteMessage = addSendTime(IncompleteMessage, SendTime),
    CompleteMessage.

addSendTime(IncompleteMessage, SendTime) ->
    CompleteMessage = lists:concat([IncompleteMessage, SendTime]),
    CompleteMessage.

getStationType(Message) ->
    [FirstLetter | _Rest] = Message,
    FirstLetter.

getStationName(Message) ->
    lists:substring(Message, 1, 12).

getSlotNumber(Message) ->
    SlotNumberLength = length(Message) - 33,
    SlotNumberString = lists:substring(Message, ?SLOTNUMBERPOS, SlotNumberLength),
    {SlotNumber, []} = string:to_integer(SlotNumberString),
    SlotNumber.