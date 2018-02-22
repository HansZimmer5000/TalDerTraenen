-module(man).

-export([
    start/0
]).

-define(KOPID, {koordinator, 'ko@HansZimmer-PC'}).

start() ->
    {ok, [CommandStr]} = io:fread("Kommando eingeben>", "~s"),
    Command = list_to_atom(CommandStr),

    case Command of
        reset ->    ?KOPID ! reset,
                    start();
        step ->     ?KOPID ! step,
                    start();
        calc ->     {ok, [WggT]} = io:fread("Bitte WggT eingeben>", "~d"),
                    case is_number(WggT) of
                        true -> ?KOPID ! {calc, WggT},
                                start();
                        false ->io:fwrite("WggT war keine Zahl.\n"),
                                start()
                    end;
        prompt ->   ?KOPID ! prompt,
                    start();
        nudge ->    ?KOPID ! nudge,
                    start();
        toggle ->   ?KOPID ! toggle,
                    start();
        kill ->     ?KOPID ! kill,
                    start();
        _Any ->   io:fwrite("Kommando unbekannt.\n"),
                    start()
    end.