
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

%------------------------------------------------------------------------------------------------------
%										>>SCHNITTSTELLEN<<
%------------------------------------------------------------------------------------------------------
% Initialisiert eine CMEM ohne Tupel (leere Liste)
initCMEM(ErinnerungsZeitSek, LogDatei) ->
    CMEM = {ErinnerungsZeitSek, []},
    logge_status("CMEM initalisiert", LogDatei),
    CMEM.

% Setzt die neue NNr fuer einen bestimmten Client in dementsprechenden Tupel (setClientNNr)
% loggt dies und gibt die neue CMEM zurueck.
updateClient({ErinnerungsZeitSek, TupelListe}, ClientPid, NNr, LogDatei) ->
    NeueTupelListe = setClientNNr(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p update zu NNR: ~p", [ClientPid, NNr]), LogDatei),
    CMEM = {ErinnerungsZeitSek, NeueTupelListe},
    CMEM.

% Setzt die neue NNr für einen bestimmten Client in dementsprechenden Tupel
% Koennte man theoretisch noch zu Endrekursiv umschreiben.
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

% Holt die gespeicherte NNr von einem bestimmten Client.
getClientNNr({_ErinnerungsZeitSek, []}, _ClientPid) -> 
    ?DEFAULT_NNR + 1;
getClientNNr({ErinnerungsZeitSek, [KopfTupel | RestTupel]}, ClientPid) ->
    case KopfTupel of
        {ClientPid, NNr, LetzterTS} -> 
            pruefe_ts_und_gib_nnr_zurueck(LetzterTS, ErinnerungsZeitSek, NNr) + 1;
        _Any -> 
            getClientNNr({ErinnerungsZeitSek, RestTupel}, ClientPid)
    end.

% Prueft ob der gesetzte Timestamp schon abgelaufen ist und gibt je nach dem die gespeicherte NNr zurueck.
pruefe_ts_und_gib_nnr_zurueck(LetzterTS, ErinnerungsZeitSek, LetzteNNr) ->
    case ts_ist_abglaufen(LetzterTS, ErinnerungsZeitSek) of
        true ->  ?DEFAULT_NNR;
        false -> LetzteNNr
    end.

% Prueft ob der Timestamp laenger her ist als es die Erinnerungszeit zulaesst.
ts_ist_abglaufen(LetzterTS, ErinnerungsZeitSek) ->
    JetztTS = erlang:timestamp(),
    {_DiffMegaSec, DiffSec, _DiffMicroSec} = vsutil:diffTS(JetztTS, LetzterTS),
    DiffSec > ErinnerungsZeitSek.

% Loescht die CMEM
delCMEM(_CMEM) ->
    ok.

%------------------------------------------------------------------------------------------------------
%											>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).
