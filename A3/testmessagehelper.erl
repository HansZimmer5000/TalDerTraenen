-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    prepareIncompleteMessageForSending/2,
%    addSendTime/2,

prepareIncompleteMessageForSending_1_test() -> 
    io:fwrite("Not implemented yet"),
    ?assert(false).
    
addSendTime_1_test() -> 
    ShouldResult = "A-team-4711-123456789012-477394825",
    TestMessage = "A-team-4711-123456789012-4",
    SendTime = "77394825",
    IsResult = messagehelper:addSendTime(TestMessage, SendTime),
    ?assertEqual(ShouldResult, IsResult).

addSendTime_2_test() -> 
    ShouldResult = "A-team-4711-123456789012-477394825",
    TestMessage = "A-team-4711-123456789012-4",
    SendTime = 77394825,
    IsResult = messagehelper:addSendTime(TestMessage, SendTime),
    ?assertEqual(ShouldResult, IsResult).
