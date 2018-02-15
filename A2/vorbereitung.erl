-module(vorbereitung).

-export([
    ggT/2,
    ggTV/2
]).

ggT(X, 0) -> X;
ggT(X, Y) ->
    NewX = Y,
    NewY = X rem Y,
    ggT(NewX, NewY).

ggTV(X, Y) -> 
    case Y < X of
        true -> NewY = ((X - 1) rem Y) + 1;
        false -> NewY = Y
    end,
    NewY.