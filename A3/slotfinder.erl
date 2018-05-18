-module(slotfinder).

-export([
    start/3,

    find_slot_in_next_frame/3,
    get_taken_slots/2,
    delete_possible_slots/2,
    select_random_slot/1
]).

-define(DEFAULT_POSSIBLE_SLOTS, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,24,25]).

% ------------------ Init --------------
start(CorePid, StationName, LogFile) ->
    logge_status("Startet", LogFile),
    ClockPid = spawn(fun() -> loop(CorePid, StationName, [], LogFile) end),
    ClockPid.
	
% ------------------ Loop --------------
loop(CorePid, StationName, Messages, LogFile) ->
 receive
	newframe -> 
            loop(CorePid, StationName, [], LogFile);
	{newmessages, ReceivedMessages} ->	
			NewMessages = ReceivedMessages ++ Messages,
			loop(CorePid, StationName, NewMessages, LogFile);
	{getFreeSlotNum, SenderPid} ->		
			NewSlotNumber = find_slot_in_next_frame(Messages, StationName, LogFile),
			SenderPid ! {slotnum, NewSlotNumber},
			loop(CorePid, StationName, Messages, LogFile)
 end.

% ------------------ Internal Functions --------------
find_slot_in_next_frame(Messages, _StationName, LogFile) ->
	TakenSlots = get_taken_slots(Messages, []),
	PossibleSlots = delete_possible_slots(?DEFAULT_POSSIBLE_SLOTS, TakenSlots),
	logge_status("TakenSlots: ~p, Possible: ~p",[TakenSlots, PossibleSlots],LogFile),
	select_random_slot(PossibleSlots).

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
logge_status(Text, Input, LogFile) ->
    Inhalt = io_lib:format(Text,Input),
    logge_status(Inhalt, LogFile).

logge_status(Inhalt, LogFile) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p -- Slot ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogFile, LogNachricht).