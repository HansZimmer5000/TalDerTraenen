-module(slotfinder).

-export([
    findSlotInNextFrame/2,
    getSlotNumberIfStationNameMatches/2
]).

-define(POSSIBLE_SLOTS, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

findSlotInNextFrame(Messages, StationName) ->
    case getSlotNumberIfStationNameMatches(Messages, StationName) of
        0 ->
            0; %Call function to go trough all messages and extract their slots to elimated more and more slotnumbers
        SlotNumber ->
            SlotNumber
    end.


getSlotNumberIfStationNameMatches([], _) ->
    0;
getSlotNumberIfStationNameMatches([HeadMessage | RestMessages], StationName) ->
    MessageStationName = messagehelper:getStationName(HeadMessage),
    case string:equal(StationName, MessageStationName) of
        true ->
            messagehelper:getSlotNumber(HeadMessage);
        false ->
            getSlotNumberIfStationNameMatches(RestMessages, StationName)
    end.