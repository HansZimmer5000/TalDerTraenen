-module(testslotfinder).

-include_lib("eunit/include/eunit.hrl").


get_slot_numer_if_stationname_matches_1_test() ->
    Messages = [],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:get_slot_numer_if_stationname_matches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

get_slot_numer_if_stationname_matches_2_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0002-","123456789012-",4, 77394825}, 77394825}],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:get_slot_numer_if_stationname_matches(Messages, StationName),
    ?assertEqual(0, FoundSlotNumber).

get_slot_numer_if_stationname_matches_3_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0000-","123456789012-",4, 77394825}, 77394825}],
    StationName = "-team-0000-",
    FoundSlotNumber = slotfinder:get_slot_numer_if_stationname_matches(Messages, StationName),
    ?assertEqual(4, FoundSlotNumber).



get_taken_slots_1_test() ->
    Messages = [
        {{"A","-team-0001-","123456789012-",4, 77394825}, 77394825}, 
        {{"A","-team-0000-","123456789012-",25, 77394825}, 77394825}],
    ?assertEqual(
        [25, 4], 
        slotfinder:get_taken_slots(Messages, [])).

get_taken_slots_2_test() ->
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
        slotfinder:get_taken_slots(Messages, [])).



delete_possible_slots_1_test() ->
    ?assertEqual([3], slotfinder:delete_possible_slots([1,2,3], [1,2])).

delete_possible_slots_2_test() ->
    ?assertEqual([1,2], slotfinder:delete_possible_slots([1,2], [])).



select_random_slot_1_test() ->
    ?assertEqual(1,slotfinder:select_random_slot([1])).

select_random_slot_2_test() ->
    RandomSlot = slotfinder:select_random_slot([1,2,3,4,5,6,7,8,9,10]),
    ?assert(lists:member(RandomSlot, [1,2,3,4,5,6,7,8,9,10])).

select_random_slot_3_test() ->
    RandomSlot = slotfinder:select_random_slot([]),
    ?assertEqual(0, RandomSlot).