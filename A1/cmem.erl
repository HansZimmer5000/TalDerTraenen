
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
    NeueTupelListe = setClientNNr(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p update zu NNR: ~p", [ClientPid, NNr]), LogDatei),
    CMEM = [ErinnerungsZeitSek, NeueTupelListe],
    CMEM.

setClientNNr(TupelListe, ClientPid, NNr) ->
    %Ohne lists machen, und ggf. hinten anhängen.
    TmpTupelListe = lists:keydelete(ClientPid, 1, TupelListe),
    [{ClientPid, NNr, erlang:timestamp()}] ++ TmpTupelListe.

getClientNNr([ErinnerungsZeitSek, TupelListe], ClientPid) ->
    GefundenesTupel = lists:keyfind(ClientPid, 1, TupelListe),
    case GefundenesTupel of
        {ClientPid, NNr, AlterTS} -> 
            AktuelleNNr = pruefeTSUndGibNNrZuruck(AlterTS, ErinnerungsZeitSek, NNr);
        _Any -> 
            AktuelleNNr = ?DEFAULT_NNR
    end,
    AktuelleNNr + 1.

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
