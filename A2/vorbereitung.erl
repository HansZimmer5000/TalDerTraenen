-module(vorbereitung).

-export([
    ggT/2
]).

ggT(X, 0) -> X;
ggT(X, Y) ->
    NewX = Y,
    NewY = X rem Y,
    ggT(NewX, NewY).
