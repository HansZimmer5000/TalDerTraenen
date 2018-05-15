-module(slotfinder).

-export([
    start/3,

    find_slot_in_next_frame/2,
    get_slot_numer_if_stationname_matches/2,
    get_taken_slots/2,
    delete_possible_slots/2,
    select_random_slot/1
]).

-define(DEFAULT_POSSIBLE_SLOTS, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).

start(CorePid, StationName, LogFile) ->
    logge_status("Startet", LogFile),
    ClockPid = spawn(fun() -> loop(CorePid, StationName, [], LogFile) end),
    ClockPid.
	
	
loop(CorePid, StationName, Messages, LogFile) ->
 receive
    newframe -> 
            loop(CorePid, StationName, [], LogFile);
	{newmessages, ReceivedMessages} ->	
			NewMessages = ReceivedMessages ++ Messages,
			loop(CorePid, StationName, NewMessages, LogFile);
	{getFreeSlotNum, SenderPid} ->			
			NewSlotNumber = find_slot_in_next_frame(Messages, StationName),
			SenderPid ! {slotnum, NewSlotNumber},
			loop(CorePid, StationName, Messages, LogFile)
 end.

find_slot_in_next_frame(Messages, StationName) ->
    case get_slot_numer_if_stationname_matches(Messages, StationName) of
        0 ->
            TakenSlots = get_taken_slots(Messages, []),
            PossibleSlots = delete_possible_slots(?DEFAULT_POSSIBLE_SLOTS, TakenSlots),
            select_random_slot(PossibleSlots);
        SlotNumber ->
            SlotNumber
    end.


get_slot_numer_if_stationname_matches([], _) ->
    0;
get_slot_numer_if_stationname_matches([HeadMessage | RestMessages], StationName) ->
    MessageStationName = messagehelper:get_station_name(HeadMessage),
    case string:equal(StationName, MessageStationName) of
        true ->
            messagehelper:get_slotnumber(HeadMessage);
        false ->
            get_slot_numer_if_stationname_matches(RestMessages, StationName)
    end.

get_taken_slots([], TakenSlots) ->
    TakenSlots;
get_taken_slots([HeadMessage | RestMessages], TakenSlots) ->
    CurrentTakenSlot = messagehelper:get_slotnumber(HeadMessage),
    NewTakenSlots = [CurrentTakenSlot |TakenSlots],
    get_taken_slots(RestMessages, NewTakenSlots).

delete_possible_slots(PossibleSlots, []) ->
    PossibleSlots;
delete_possible_slots(PossibleSlots, [TakenHeadSlot | TakenRestSlots]) ->
    NewPossibleSlots = lists:delete(TakenHeadSlot, PossibleSlots),
    delete_possible_slots(NewPossibleSlots, TakenRestSlots).

select_random_slot(PossibleSlots) ->
    case length(PossibleSlots) of
        0 ->
            io:fwrite("No RandomSlot can't be selected, since list is empty"),
            0;
        PossibleSlotsLength ->
            RandomIndex = rand:uniform(PossibleSlotsLength),
            [RandomSlot] = lists:sublist(PossibleSlots, RandomIndex, 1),
            RandomSlot
    end.
    
%------------------------------------------
logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p -- Slot ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).