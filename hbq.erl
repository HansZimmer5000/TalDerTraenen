
-module(hbq).

% API
-export([start/0]).

% CONSTANTS
-define(CONSTANT1, 3).

start() ->
    io:fwrite("hbq start").