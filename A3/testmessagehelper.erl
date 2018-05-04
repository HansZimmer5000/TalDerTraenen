-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    convertReceivedMessagesFromByte/1,
%    convertMessageFromByte/1,

%    createIncompleteMessage/3,

%    prepareIncompleteMessageForSending/2,
%    setSendTimeAndPayload/2,
%    convertMessageToByte/1,

%    getStationType/1,

%    getStationName/1,

%    getSlotNumber/1

% <<0,0,1,98,108,153,173,43>> == 1522240433451.
-define(DEFAULT_FULL_MESSAGE_4, {{"A", "-team-0602-", "123456789012-", 4, 1522240433451}, empty}).
-define(DEFAULT_FULL_MESSAGE_25, {{"A", "-team-0602-", "123456789012-", 25, 1522240433451}, empty}).
-define(DEFAULT_FULL_RECEIVED_MESSAGE_4, {{"A", "-team-0602-", "123456789012-", 4, 1522240433451}, 1522240433451}).
-define(DEFAULT_FULL_RECEIVED_MESSAGE_25, {{"A", "-team-0602-", "123456789012-", 25, 1522240433451}, 1522240433451}).
-define(MESSAGE_AS_BINARY_LENGTH, 34).

convertReceivedMessagesFromByte_1_test() ->
    SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
    Message1AsByte = <<"A-team-0602-123456789012-", 4, SendTimeBinary/binary>>,
    Message2AsByte = <<"A-team-0602-123456789012-", 25, SendTimeBinary/binary>>,
    ReceivedTimes = [1522240433451, 1522240433451],
    [Message2, Message1] = messagehelper:convertReceivedMessagesFromByte([Message1AsByte, Message2AsByte], ReceivedTimes),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(Message1AsByte)),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(Message2AsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_4, Message1),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_25, Message2).

convertReceivedMessagesFromByte_2_test() ->
    ?assertEqual([], messagehelper:convertReceivedMessagesFromByte([], [])).

convertMessageFromByte_1_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0602-123456789012-">>,
    SlotNumber = 4,
    SendTime = <<0,0,1,98,108,153,173,43>>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ReceivedTime = 1522240433451,
    ConvertedMessage = messagehelper:convertMessageFromByte(MessageAsByte, ReceivedTime),

    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(MessageAsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_4, ConvertedMessage).

convertMessageFromByte_2_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0602-123456789012-">>,
    SlotNumber = 25,
    SendTime = <<0,0,1,98,108,153,173,43>>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ReceivedTime = 1522240433451,
    ConvertedMessage = messagehelper:convertMessageFromByte(MessageAsByte, ReceivedTime),

    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(MessageAsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_25, ConvertedMessage).

createIncompleteMessage_1_test() ->
    StationType = "A",
    SlotNumber = 4,
    IncompleteMessage = messagehelper:createIncompleteMessage(StationType, SlotNumber),
    ?assertEqual({{"A",empty, empty, 4, empty}, empty}, IncompleteMessage).

prepareIncompleteMessageForSending_1_test() -> 
    IncompleteMessage = {{"A",empty, empty, 4, empty}, empty},
    SendTime = 1522240433451,
    SendTime8ByteBinary = <<0,0,1,98,108,153,173,43>>,
    Message = messagehelper:prepareIncompleteMessageForSending(IncompleteMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(<<"A-team-0602-123456789012-", 4, SendTime8ByteBinary/binary>>, Message).

prepareIncompleteMessageForSending_2_test() -> 
    IncompleteMessage = {{"A",empty, empty, 25, empty}, empty},
    SendTime = 1522240433451,
    SendTime8ByteBinary = <<0,0,1,98,108,153,173,43>>,
    Message = messagehelper:prepareIncompleteMessageForSending(IncompleteMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(<<"A-team-0602-123456789012-", 25, SendTime8ByteBinary/binary>>, Message).

setSendTimeAndPayload_1_test() -> 
    ShouldResult = {{"A","-team-0602-", "123456789012-", 25, 1522240433451}, empty},
    TestMessage = {{"A",empty, empty, 25, empty}, empty},
    SendTime = 1522240433451,
    IsResult = messagehelper:setSendTimeAndPayload(TestMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(ShouldResult, IsResult).

convertMessageToByte_1_test() ->
    SendTime = <<0,0,1,98,108,153,173,43>>,
    Message = {{"A","-team-0602-", "123456789012-", 4, 1522240433451}, empty},
    ConvertedMessage = messagehelper:convertMessageToByte(Message),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0602-123456789012-", 4, SendTime/binary>>,
        ConvertedMessage).

convertMessageToByte_2_test() ->
    SendTime = <<0,0,1,98,108,153,173,43>>,
    Message = {{"A","-team-0602-", "123456789012-", 25, 1522240433451}, 1522240433451},
    ConvertedMessage = messagehelper:convertMessageToByte(Message),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0602-123456789012-", 25, SendTime/binary>>,
        ConvertedMessage).

getStationType_1_test() -> 
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    StationType = messagehelper:getStationType(Message),
    ?assertEqual("A", StationType).

getStationName_1_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    StationType = messagehelper:getStationName(Message),
    ?assertEqual("-team-0602-", StationType).

getSlotNumber_1_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_4,
    SlotNumber = messagehelper:getSlotNumber(Message),
    ?assertEqual(4, SlotNumber).

getSlotNumber_2_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    SlotNumber = messagehelper:getSlotNumber(Message),
    ?assertEqual(25, SlotNumber).
