-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    prepareIncompleteMessageForSending/2,
%    addSendTime/2,

%    getStationType/1,

%    getStationName/1,

%    getSlotNumber/1

prepareIncompleteMessageForSending_1_test() -> 
    IncompleteMessage = "A-team-0000-123456789012-4",
    SendTime = 77394825,
    Message = messagehelper:prepareIncompleteMessageForSending(IncompleteMessage, SendTime),
    ?assertEqual("A-team-0000-123456789012-477394825", Message).

    
addSendTime_1_test() -> 
    ShouldResult = "A-team-0000-123456789012-477394825",
    TestMessage = "A-team-0000-123456789012-4",
    SendTime = "77394825",
    IsResult = messagehelper:addSendTime(TestMessage, SendTime),
    ?assertEqual(ShouldResult, IsResult).

addSendTime_2_test() -> 
    ShouldResult = "A-team-0000-123456789012-477394825",
    TestMessage = "A-team-0000-123456789012-4",
    SendTime = 77394825,
    IsResult = messagehelper:addSendTime(TestMessage, SendTime),
    ?assertEqual(ShouldResult, IsResult).

getStationType_1_test() -> 
    Message = "A-team-0000-123456789012-477394825",
    StationType = messagehelper:getStationType(Message),
    ?assertEqual("A",StationType).

getStationName_1_test() ->
    Message = "A-team-0000-123456789012-477394825",
    StationType = messagehelper:getStationName(Message),
    ?assertEqual("-team-0000-",StationType).

getSlotNumber_1_test() ->
    Message = "A-team-0000-123456789012-477394825",
    StationType = messagehelper:getSlotNumber(Message),
    ?assertEqual(4,StationType).

getSlotNumber_2_test() ->
    Message = "A-team-0000-123456789012-2577394825",
    StationType = messagehelper:getSlotNumber(Message),
    ?assertEqual(25,StationType).