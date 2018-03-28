-module(utcclock).

-export([
    init/1,

    adjust/2,

    get8ByteUTCBinary/1
]).

%TODO: Problem!
% SendTime should be 8 Byte, but vsutil:getUTC() return Number of length 13.
% binary:encode_unsigned(vsutil:getUTC(), big) -> converts int to binary and is leaving us with 6 byte length.

init(_OffsetMS) ->
    io:fwrite("not implemented yet"),
    empty.


adjust(_UTCClock, _Messages) ->
    io:fwrite("not implemented yet"),
    empty.

get8ByteUTCBinary(ErlangTS) ->
    TSAsUTC = vsutil:now2UTC(ErlangTS),
    Tmp = binary:encode_unsigned(TSAsUTC, big),
    <<0,0, Tmp/binary>>.