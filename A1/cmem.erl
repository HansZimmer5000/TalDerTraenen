
-module(cmem).

% API
-export([
    initCMEM/2,
    updateClient/4,
    getClientNNr/2,
    pruefeTSUndGibNNrZuruck/3,
    tSIstAbglaufen/2
]).


initCMEM(ErinnerungsZeit, LogDatei) ->
    CMEM = [ErinnerungsZeit, []],
    logge_status("CMEM initalisiert", LogDatei),
    CMEM.


updateClient([Zeit, TupelListe], ClientPid, NNr, LogDatei) ->
    NeueTupelListe = updateClient_(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p bekommt als nächstes NNr ~p", [ClientPid, NNr]), LogDatei),
    CMEM = [Zeit, NeueTupelListe],
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

getClientNNr([Zeit, TupelListe], ClientPid) ->
    NNr = getClientNNr_(TupelListe, Zeit, ClientPid),
    NNr.

getClientNNr_([], _Zeit, _ClientPid) -> 1;
getClientNNr_([KopfTupel | RestTupel], Zeit, ClientPid) ->
    case KopfTupel of
        {ClientPid, NNr, OldTS} -> pruefeTSUndGibNNrZuruck(OldTS, Zeit, NNr);
        _Any -> getClientNNr_(RestTupel, Zeit, ClientPid)
    end.

pruefeTSUndGibNNrZuruck(OldTS, Zeit, SavedNNr) ->
    case tSIstAbglaufen(OldTS, Zeit) of
        true ->  getClientNNr_([], ok, ok);
        false -> SavedNNr
    end.

tSIstAbglaufen(OldTS, Zeit) ->
    JetztTS = erlang:timestamp(),
    {_DiffMegaSec, DiffSec, _DiffMicroSec} = vsutil:diffTS(JetztTS, OldTS),
    DiffSec > Zeit.


logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).
