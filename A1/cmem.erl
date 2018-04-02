
-module(cmem).

% API
-export([
    initCMEM/2,
    updateClient/4,
    getClientNNr/2,
    pruefe_ts_und_gib_nnr_zurueck/3,
    ts_ist_abglaufen/2,

    delCMEM/1
]).

-define(DEFAULT_NNR, 0).


initCMEM(ErinnerungsZeitSek, LogDatei) ->
    CMEM = {ErinnerungsZeitSek, []},
    logge_status("CMEM initalisiert", LogDatei),
    CMEM.


updateClient({ErinnerungsZeitSek, TupelListe}, ClientPid, NNr, LogDatei) ->
    NeueTupelListe = setClientNNr(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p update zu NNR: ~p", [ClientPid, NNr]), LogDatei),
    CMEM = {ErinnerungsZeitSek, NeueTupelListe},
    CMEM.


setClientNNr([], ClientPid, NNr) ->
    [{ClientPid, NNr, erlang:timestamp()}];
setClientNNr(TupelListe, ClientPid, NNr) ->
    [KopfTupel | RestTupel] = TupelListe,
    case KopfTupel of
        {ClientPid, _AltNNr, _AltTS} -> NeuesTupel = {ClientPid, NNr, erlang:timestamp()},
                                        NeueTupelListe = [NeuesTupel | RestTupel];
        _Any -> NeueRestTupel = setClientNNr(RestTupel, ClientPid, NNr),
                NeueTupelListe = [KopfTupel | NeueRestTupel]
    end,
    NeueTupelListe.

getClientNNr({_ErinnerungsZeitSek, []}, _ClientPid) -> 
    ?DEFAULT_NNR + 1;
getClientNNr({ErinnerungsZeitSek, [KopfTupel | RestTupel]}, ClientPid) ->
    case KopfTupel of
        {ClientPid, NNr, LetzterTS} -> 
            pruefe_ts_und_gib_nnr_zurueck(LetzterTS, ErinnerungsZeitSek, NNr) + 1;
        _Any -> 
            getClientNNr({ErinnerungsZeitSek, RestTupel}, ClientPid)
    end.

pruefe_ts_und_gib_nnr_zurueck(LetzterTS, ErinnerungsZeitSek, LetzteNNr) ->
    case ts_ist_abglaufen(LetzterTS, ErinnerungsZeitSek) of
        true ->  ?DEFAULT_NNR;
        false -> LetzteNNr
    end.

ts_ist_abglaufen(LetzterTS, ErinnerungsZeitSek) ->
    JetztTS = erlang:timestamp(),
    {_DiffMegaSec, DiffSec, _DiffMicroSec} = vsutil:diffTS(JetztTS, LetzterTS),
    DiffSec > ErinnerungsZeitSek.

% Nicht in der Aufgabenstellung!
% Dennoch vom Gegebenen Server verlangt.
delCMEM(_CMEM) ->
    ok.


logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).
