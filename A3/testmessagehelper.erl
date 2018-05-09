-module(testmessagehelper).

-include_lib("eunit/include/eunit.hrl").

%    convert_received_messages_from_byte/1,
%    convert_message_from_byte/1,

%    create_incomplete_message/3,

%    prepare_incomplete_message_for_sending/2,
%    set_sendtime_and_payload/2,
%    convert_message_to_byte/1,

%    get_station_type/1,

%    get_station_name/1,

%    get_slotnumber/1

% <<0,0,1,98,108,153,173,43>> == 1522240433451.
-define(DEFAULT_FULL_MESSAGE_4, {{"A", "-team-0602-", "123456789012-", 4, 1522240433451}, empty}).
-define(DEFAULT_FULL_MESSAGE_25, {{"A", "-team-0602-", "123456789012-", 25, 1522240433451}, empty}).
-define(DEFAULT_FULL_RECEIVED_MESSAGE_4, {{"A", "-team-0602-", "123456789012-", 4, 1522240433451}, 1522240433451}).
-define(DEFAULT_FULL_RECEIVED_MESSAGE_25, {{"A", "-team-0602-", "123456789012-", 25, 1522240433451}, 1522240433451}).
-define(MESSAGE_AS_BINARY_LENGTH, 34).

convert_received_messages_from_byte_1_test() ->
    SendTimeBinary = <<0,0,1,98,108,153,173,43>>,
    Message1AsByte = <<"A-team-0602-123456789012-", 4, SendTimeBinary/binary>>,
    Message2AsByte = <<"A-team-0602-123456789012-", 25, SendTimeBinary/binary>>,
    ReceivedTimes = [1522240433451, 1522240433451],
    [Message2, Message1] = messagehelper:convert_received_messages_from_byte([Message1AsByte, Message2AsByte], ReceivedTimes),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(Message1AsByte)),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(Message2AsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_4, Message1),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_25, Message2).

convert_received_messages_from_byte_2_test() ->
    ?assertEqual([], messagehelper:convert_received_messages_from_byte([], [])).

convert_message_from_byte_1_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0602-123456789012-">>,
    SlotNumber = 4,
    SendTime = <<0,0,1,98,108,153,173,43>>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ReceivedTime = 1522240433451,
    ConvertedMessage = messagehelper:convert_message_from_byte(MessageAsByte, ReceivedTime),

    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(MessageAsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_4, ConvertedMessage).

convert_message_from_byte_2_test() ->
    StationType = <<"A">>,
    Payload = <<"-team-0602-123456789012-">>,
    SlotNumber = 25,
    SendTime = <<0,0,1,98,108,153,173,43>>,
    MessageAsByte = <<StationType/binary, Payload/binary, SlotNumber, SendTime/binary>>,
    ReceivedTime = 1522240433451,
    ConvertedMessage = messagehelper:convert_message_from_byte(MessageAsByte, ReceivedTime),

    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(MessageAsByte)),
    ?assertEqual(?DEFAULT_FULL_RECEIVED_MESSAGE_25, ConvertedMessage).

create_incomplete_message_1_test() ->
    StationType = "A",
    SlotNumber = 4,
    IncompleteMessage = messagehelper:create_incomplete_message(StationType, SlotNumber),
    ?assertEqual({{"A",empty, empty, 4, empty}, empty}, IncompleteMessage).

prepare_incomplete_message_for_sending_1_test() -> 
    IncompleteMessage = {{"A",empty, empty, 4, empty}, empty},
    SendTime = 1522240433451,
    SendTime8ByteBinary = <<0,0,1,98,108,153,173,43>>,
    Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(<<"A-team-0602-123456789012-", 4, SendTime8ByteBinary/binary>>, Message).

prepare_incomplete_message_for_sending_2_test() -> 
    IncompleteMessage = {{"A",empty, empty, 25, empty}, empty},
    SendTime = 1522240433451,
    SendTime8ByteBinary = <<0,0,1,98,108,153,173,43>>,
    Message = messagehelper:prepare_incomplete_message_for_sending(IncompleteMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(<<"A-team-0602-123456789012-", 25, SendTime8ByteBinary/binary>>, Message).

set_sendtime_and_payload_1_test() -> 
    ShouldResult = {{"A","-team-0602-", "123456789012-", 25, 1522240433451}, empty},
    TestMessage = {{"A",empty, empty, 25, empty}, empty},
    SendTime = 1522240433451,
    IsResult = messagehelper:set_sendtime_and_payload(TestMessage, SendTime, "-team-0602-123456789012-"),
    ?assertEqual(ShouldResult, IsResult).

convert_message_to_byte_1_test() ->
    SendTime = <<0,0,1,98,108,153,173,43>>,
    Message = {{"A","-team-0602-", "123456789012-", 4, 1522240433451}, empty},
    ConvertedMessage = messagehelper:convert_message_to_byte(Message),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0602-123456789012-", 4, SendTime/binary>>,
        ConvertedMessage).

convert_message_to_byte_2_test() ->
    SendTime = <<0,0,1,98,108,153,173,43>>,
    Message = {{"A","-team-0602-", "123456789012-", 25, 1522240433451}, 1522240433451},
    ConvertedMessage = messagehelper:convert_message_to_byte(Message),
    ?assertEqual(?MESSAGE_AS_BINARY_LENGTH, byte_size(ConvertedMessage)),
    ?assertEqual(
        <<"A-team-0602-123456789012-", 25, SendTime/binary>>,
        ConvertedMessage).

get_station_type_1_test() -> 
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    StationType = messagehelper:get_station_type(Message),
    ?assertEqual("A", StationType).

get_station_name_1_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    StationType = messagehelper:get_station_name(Message),
    ?assertEqual("-team-0602-", StationType).

get_slotnumber_1_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_4,
    SlotNumber = messagehelper:get_slotnumber(Message),
    ?assertEqual(4, SlotNumber).

get_slotnumber_2_test() ->
    Message = ?DEFAULT_FULL_RECEIVED_MESSAGE_25,
    SlotNumber = messagehelper:get_slotnumber(Message),
    ?assertEqual(25, SlotNumber).
