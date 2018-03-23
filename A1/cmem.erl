
-module(cmem).

% API
-export([
    initCMEM/2,
    updateClient/4,
    getClientNNr/2,
    pruefeTSUndGibNNrZuruck/3,
    tSIstAbglaufen/2,

    delCMEM/1
]).

-define(DEFAULT_NNR, 0).


initCMEM(ErinnerungsZeitSek, LogDatei) ->
    CMEM = [ErinnerungsZeitSek, []],
    logge_status("CMEM initalisiert", LogDatei),
    CMEM.


updateClient([ErinnerungsZeitSek, TupelListe], ClientPid, NNr, LogDatei) ->
    NeueTupelListe = updateClient_(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p update zu NNR: ~p", [ClientPid, NNr]), LogDatei),
    CMEM = [ErinnerungsZeitSek, NeueTupelListe],
    CMEM.

updateClient_([], ClientPid, NNr) ->
    [{ClientPid, NNr, erlang:timestamp()}];
updateClient_(TupelListe, ClientPid, NNr) ->
    [KopfTupel | RestTupel] = TupelListe,
    case KopfTupel of
        {ClientPid, _AltNNr, _AltTS} -> NeuesTupel = {ClientPid, NNr, erlang:timestamp()},
                                        NeueTupelListe = [NeuesTupel | RestTupel];
        _Any -> NeueRestTupel = updateClient_(RestTupel, ClientPid, NNr),
                NeueTupelListe = [KopfTupel | NeueRestTupel]
    end,
    NeueTupelListe.

getClientNNr([ErinnerungsZeitSek, TupelListe], ClientPid) ->
    NNr = getClientNNr_(TupelListe, ErinnerungsZeitSek, ClientPid),
    NNr.

getClientNNr_([], _ErinnerungsZeitSek, _ClientPid) -> ?DEFAULT_NNR;
getClientNNr_([KopfTupel | RestTupel], ErinnerungsZeitSek, ClientPid) ->
    case KopfTupel of
        {ClientPid, NNr, OldTS} -> pruefeTSUndGibNNrZuruck(OldTS, ErinnerungsZeitSek, NNr);
        _Any -> getClientNNr_(RestTupel, ErinnerungsZeitSek, ClientPid)
    end.

pruefeTSUndGibNNrZuruck(OldTS, ErinnerungsZeitSek, SavedNNr) ->
    case tSIstAbglaufen(OldTS, ErinnerungsZeitSek) of
        true ->  ?DEFAULT_NNR;
        false -> SavedNNr
    end.

tSIstAbglaufen(OldTS, ErinnerungsZeitSek) ->
    JetztTS = erlang:timestamp(),
    {_DiffMegaSec, DiffSec, _DiffMicroSec} = vsutil:diffTS(JetztTS, OldTS),
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
