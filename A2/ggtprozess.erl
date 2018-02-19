-module(ggtprozess).

-export([
    ggTV/2
]).

go({ArbeitsZeit, TermZeit, GGTProName, Quota}) ->
    ok.

init_receive_loop() ->
    receive
        {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProAnz} -> none
        {setneighbors, LeftN, RightN} -> none 
        {setpm, MiNeu} -> none
    end.

receive_loop() ->
    receive
        {setpm, MiNeu} -> none
        {sendy, Y} -> {sendy, MiNeu}
        {briefmi, {GGTProName, CMi, CZeit}} -> none
        {AbsenderPid, briefterm, {GGTProName, CMi, CZeit}} -> none
        {voteYes, GGTProName} -> none
        {AbsenderPid, tellmi} -> {mi, Mi}
        {AbsenderPid, pingGGT} -> {pongGGT, GGTProName}
        kill -> none
    end.


ggTV(X, Y) -> 
    case Y < X of
        true -> NewY = ((X - 1) rem Y) + 1;
        false -> NewY = Y
    end,
    NewY.