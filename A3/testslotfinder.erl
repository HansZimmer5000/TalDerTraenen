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