-module(start_start_starter).


-export([start/1]).

start(ParameterFromPython) ->
    [StarterAnzahlAtom, ErsteStarterNummerAtom] = ParameterFromPython,
    
    {StarterAnzahl, []} = string:to_integer(atom_to_list(StarterAnzahlAtom)),
    {ErsteStarterNummer, []} = string:to_integer(atom_to_list(ErsteStarterNummerAtom)),
    io:fwrite("Starte Starter mit ~p (Anzahl) und ~p (ErsteStarterNummer)", [StarterAnzahl, ErsteStarterNummer]),
    start_starter:go(StarterAnzahl, ErsteStarterNummer).