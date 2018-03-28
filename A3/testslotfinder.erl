-module(testslotfinder).

-include_lib("eunit/include/eunit.hrl").


getSlotNumberIfStationNameMatches_1_test() ->
    Messages = [],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

getSlotNumberIfStationNameMatches_2_test() ->
    Messages = ["A-team-0001-123456789012-477394825", "A-team-0002-123456789012-477394825"],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

getSlotNumberIfStationNameMatches_3_test() ->
    Messages = ["A-team-0001-123456789012-477394825", "A-team-0000-123456789012-477394825"],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(4, FoundSlotNumber).



getTakenSlots_1_test() ->
    Messages = [
        "A-team-0001-123456789012-477394825", 
        "A-team-0002-123456789012-2577394825"],
    ?assertEqual(
        [25, 4], 
        slotfinder:getTakenSlots(Messages, [])).

getTakenSlots_2_test() ->
    Messages = [
        "A-team-0001-123456789012-177394825", 
        "A-team-0002-123456789012-277394825", 
        "A-team-0002-123456789012-377394825", 
        "A-team-0002-123456789012-477394825", 
        "A-team-0002-123456789012-577394825", 
        "A-team-0002-123456789012-677394825", 
        "A-team-0002-123456789012-777394825", 
        "A-team-0002-123456789012-877394825", 
        "A-team-0002-123456789012-977394825", 
        "A-team-0002-123456789012-1077394825", 
        "A-team-0002-123456789012-1177394825", 
        "A-team-0002-123456789012-1277394825", 
        "A-team-0002-123456789012-1377394825", 
        "A-team-0002-123456789012-1477394825", 
        "A-team-0002-123456789012-1577394825", 
        "A-team-0002-123456789012-1677394825",
        "A-team-0002-123456789012-1777394825", 
        "A-team-0002-123456789012-1877394825", 
        "A-team-0002-123456789012-1977394825", 
        "A-team-0002-123456789012-2077394825", 
        "A-team-0002-123456789012-2177394825", 
        "A-team-0002-123456789012-2277394825", 
        "A-team-0002-123456789012-2377394825", 
        "A-team-0002-123456789012-2477394825", 
        "A-team-0002-123456789012-2577394825"],
    ?assertEqual(
        [25,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1], 
        slotfinder:getTakenSlots(Messages, [])).



deletePossibleSlots_1_test() ->
    ?assertEqual([3], slotfinder:deletePossibleSlots([1,2,3], [1,2])).

deletePossibleSlots_2_test() ->
    ?assertEqual([1,2], slotfinder:deletePossibleSlots([1,2], [])).



selectRandomSlot_1_test() ->
    ?assertEqual(1,slotfinder:selectRandomSlot([1])).

selectRandomSlot_2_test() ->
    RandomSlot = slotfinder:selectRandomSlot([1,2,3,4,5,6,7,8,9,10]),
    ?assert(lists:member(RandomSlot, [1,2,3,4,5,6,7,8,9,10])).

selectRandomSlot_3_test() ->
    RandomSlot = slotfinder:selectRandomSlot([]),
    ?assertEqual(0, RandomSlot).