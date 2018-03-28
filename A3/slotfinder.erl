-module(slotfinder).

-export([
    findSlotInNextFrame/2,
    getSlotNumberIfStationNameMatches/2,
    getTakenSlots/2,
    deletePossibleSlots/2,
    selectRandomSlot/1
]).

-define(DEFAULT_POSSIBLE_SLOTS, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

findSlotInNextFrame(Messages, StationName) ->
    case getSlotNumberIfStationNameMatches(Messages, StationName) of
        0 ->
            TakenSlots = getTakenSlots(Messages, []),
            PossibleSlots = deletePossibleSlots(?DEFAULT_POSSIBLE_SLOTS, TakenSlots),
            selectRandomSlot(PossibleSlots);
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

getTakenSlots([], TakenSlots) ->
    TakenSlots;
getTakenSlots([HeadMessage | RestMessages], TakenSlots) ->
    CurrentTakenSlot = messagehelper:getSlotNumber(HeadMessage),
    NewTakenSlots = [CurrentTakenSlot |TakenSlots],
    getTakenSlots(RestMessages, NewTakenSlots).

deletePossibleSlots(PossibleSlots, []) ->
    PossibleSlots;
deletePossibleSlots(PossibleSlots, [TakenHeadSlot | TakenRestSlots]) ->
    NewPossibleSlots = lists:delete(TakenHeadSlot, PossibleSlots),
    deletePossibleSlots(NewPossibleSlots, TakenRestSlots).

selectRandomSlot(PossibleSlots) ->
    case length(PossibleSlots) of
        0 ->
            io:fwrite("No RandomSlot can't be selected, since list is empty"),
            0;
        PossibleSlotsLength ->
            RandomIndex = rand:uniform(PossibleSlotsLength),
            [RandomSlot] = lists:sublist(PossibleSlots, RandomIndex, 1),
            RandomSlot
    end.

    