-module(testslotfinder).

-include_lib("eunit/include/eunit.hrl").


getSlotNumberIfStationNameMatches_1_test() ->
    Messages = [],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

getSlotNumberIfStationNameMatches_2_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0002-","123456789012-",4, 77394825}, 77394825}],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

getSlotNumberIfStationNameMatches_3_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0000-","123456789012-",4, 77394825}, 77394825}],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:getSlotNumberIfStationNameMatches(Messages, StationName),
    ?assertEqual(4, FoundSlotNumber).



getTakenSlots_1_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0000-","123456789012-",25, 77394825}, 77394825}],
    ?assertEqual(
        [25, 4], 
        slotfinder:getTakenSlots(Messages, [])).

getTakenSlots_2_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",1, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",2, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",3, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",5, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",6, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",7, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",8, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",9, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",10, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",11, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",12, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",13, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",14, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",15, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",16, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",17, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",18, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",19, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",20, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",21, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",22, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",23, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",24, 77394825}, 77394825}, 
        {{"A","-team-0001-","123456789012-",25, 77394825}, 77394825}],
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