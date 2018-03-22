-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    convertMessagesFromByte/1,
%    convertMessagesFromByte/2,

%    prepareForSending/2,
%    addSendTime/2,
%    convertMessageToByte/1

convertMessagesFromByte_1_test() -> 
    io:fwrite("Not implemented yet"),
    ?assert(false).

convertMessagesFromByte_2_test() -> 
    io:fwrite("Not implemented yet"),
    ?assert(false).

prepareForSending_1_test() -> 
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

convertMessageToByte_1_test() -> 
    ShouldResult = <<"A-team-4711-123456789012-477394825">>,
    TestMessage = "A-team-4711-123456789012-477394825",
    IsResult = messagehelper:convertMessageToByte(TestMessage),
    ?assertEqual(ShouldResult, IsResult).

convertMessageToByte_2_test() -> 
    ShouldResult = <<
        65,
        45,116,101,97,109,45,52,55,49,49,45,59,50,51,52,53,54,55,56,57,48,49,50,45,
        25,
        55,55,52,57,52,56,50,53
    >>,
    TestMessage = "A-team-4711-123456789012-25 77394825",
    IsResult = messagehelper:convertMessageToByte(TestMessage),
    io:fwrite("~p", [IsResult == ShouldResult]),
    ?assertEqual(ShouldResult, IsResult).

convertMessageToByte_3_test() -> 
    ShouldResult = 34,
    TestMessage = "A-team-4711-123456789012-2577394825",
    IsResult = messagehelper:convertMessageToByte(TestMessage),
    ?assertEqual(ShouldResult, byte_size(IsResult)).

convertMessageToByte_4_test() -> 
    ShouldResult = 34,
    TestMessage = "A-team-4711-123456789012-477394825",
    IsResult = messagehelper:convertMessageToByte(TestMessage),
    ?assertEqual(ShouldResult, byte_size(IsResult)).
