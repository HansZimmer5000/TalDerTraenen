
-module(cmem).

% API
-export([
    initCMEM/2,
    updateClient/4,
    getClientNNr/2
]).

% CONSTANT
-define(ETS_TABELLENNAME, 'cmem_table').
-define(ERINNERUNGSZEIT_KEY, erinnerungsZeit).



initCMEM(ErinnerungsZeit, LogDatei) ->
    ets:new(?ETS_TABELLENNAME, [named_table, public, set]),
    ets:insert(?ETS_TABELLENNAME, {erinnerungsZeit, ErinnerungsZeit}),
    logge_status("CMEM initalisiert", LogDatei).


updateClient(_CMEM, ClientPid, NNR, LogDatei) ->
    ets:insert(?ETS_TABELLENNAME, {ClientPid, NNR, erlang:timestamp()}),
    logge_status(io_lib:format("~p bekommt als nächstes NNR ~p", [ClientPid, NNR]), LogDatei).


getClientNNr(_CMEM, ClientPid) ->   
    Ergebnis = ets:lookup(?ETS_TABELLENNAME, ClientPid),
    case Ergebnis of
        [{_Any, NNR}] -> NNR;
        _Any -> 1
    end.




logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).
