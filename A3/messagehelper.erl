-module(messagehelper).


-export([
    prepareForSending/2,
    addSendTime/2
]).

-define(SLOTNUMBERPOS, 25).
-define(SENDTIMELENGTH, 8).

%Nachrichtenaufbau:
%    Gesamt 34 Byte // TTL = 1!
%    - Byte 0        A oder B    Stationsklasse
%    - Byte 1-24     -team-4711- Nutzdaten
%    - Byte 25       4           reservierte Slotnummer für den nächsten Frame!
%    - Byte 26-33    77394825    Zeit (gesendet) in ms seit 01.01.1970, 8-Byte Integer, Big Endian



prepareForSending(Message, SendTime) ->
    CompleteMessage = addSendTime(Message, SendTime),
    CompleteMessageAsByte = convertMessageToByte(CompleteMessage),
    CompleteMessageAsByte.

addSendTime(Message, SendTime) ->
    NewMessage = lists:concat([Message, SendTime]),
    NewMessage.