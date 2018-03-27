-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    convertMessagesFromByte/1,
%    convertMessageFromByte/1,

%    createIncompleteMessage/3,

%    prepareIncompleteMessageForSending/2,
%    addSendTime/2,
%    convertMessageToByte/1,

%    getStationType/1,

%    getStationName/1,

%    getSlotNumber/1

convertMessagesFromByte_1_test() ->
    Message1AsByte = <<"A-team-0000-123456789012-", 4, "77394825">>,
    Message2AsByte = <<"A-team-0000-123456789012-", 25, "77394825">>,
    [Message2AsString, Message1AsString] = messagehelper:convertMessagesFromByte([Message1AsByte, Message2AsByte]),
    ?assertEqual(34, binary:referenced_byte_size(Message1AsByte)),
    ?assertEqual(34, binary:referenced_byte_size(Message2AsByte)),
    ?assert(string:equal("A-team-0000-123456789012-477394825", Message1AsString)),
    ?assert(string:equal("A-team-0000-123456789012-2577394825", Message2AsString)).

convertMessagesFromByte_2_test() ->
    ?assertEqual([], messagehelper:convertMessagesFromByte([])).

convertMessageFromByte_1_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0000-123456789012-">>,
    SlotNumber = 4,
    SendTime = <<"77394825">>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ConvertedMessage = messagehelper:convertMessageFromByte(MessageAsByte),

    ?assertEqual(34, binary:referenced_byte_size(MessageAsByte)),
    ?assert(string:equal("A-team-0000-123456789012-477394825", ConvertedMessage)).

convertMessageFromByte_2_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0000-123456789012-">>,
    SlotNumber = 25,
    SendTime = <<"77394825">>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ConvertedMessage = messagehelper:convertMessageFromByte(MessageAsByte),

    ?assertEqual(34, binary:referenced_byte_size(MessageAsByte)),
    ?assert(string:equal("A-team-0000-123456789012-2577394825", ConvertedMessage)).

createIncompleteMessage_1_test() ->
    StationType = "A",
    StationName = "-team-0000-",
    SlotNumber = 4,
    IncompleteMessage = messagehelper:createIncompleteMessage(StationType, StationName, SlotNumber),
    ?assertEqual("A-team-0000-4", IncompleteMessage).

createIncompleteMessage_2_test() ->
    io:fwrite("Fehlende Vessel3 Anbindung noch nicht implementiert & getestet!!"),
    ?assert(false).

prepareIncompleteMessageForSending_1_test() -> 
    IncompleteMessage = "A-team-0000-123456789012-4",
    SendTime = 77394825,
    Message = messagehelper:prepareIncompleteMessageForSending(IncompleteMessage, SendTime),
    ?assertEqual("A-team-0000-123456789012-477394825", Message).

prepareIncompleteMessageForSending_2_test() -> 
    IncompleteMessage = "A-team-0000-123456789012-4",
    SendTime = "77394825",
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

convertMessageToByte_1_test() ->
    Message = "A-team-0000-123456789012-477394825",
    ConvertedMessage = messagehelper:convertMessageToByte(Message),
    ?assertEqual(34, binary:referenced_byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0000-123456789012-", 4, "77394825">>,
        ConvertedMessage).

convertMessageToByte_2_test() ->
    Message = "A-team-0000-123456789012-2577394825",
    ConvertedMessage = messagehelper:convertMessageToByte(Message),
    ?assertEqual(34, binary:referenced_byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0000-123456789012-", 25, "77394825">>,
        ConvertedMessage).

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
