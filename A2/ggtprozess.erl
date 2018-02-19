-module(ggtprozess).

-export([
    ggTV/2
]).

ggTV(X, Y) -> 
    case Y < X of
        true -> NewY = ((X - 1) rem Y) + 1;
        false -> NewY = Y
    end,
    NewY.