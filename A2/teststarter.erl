-module(teststarter).

-include_lib("eunit/include/eunit.hrl").

%    go/1,

%    start_all_ggtprozesse/2,

%    create_ggtproname/2


create_ggtproname_1_test() -> 
    ?assertEqual('ggt-1263', starter:create_ggtproname(1, 3)).
