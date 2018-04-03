
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
% Initialisiert eine CMEM ohne Tupel (leere Liste wie beschrieben)
initCMEM(ErinnerungsZeitSek, LogDatei) ->
    CMEM = {ErinnerungsZeitSek, []},
    logge_status("CMEM initalisiert", LogDatei),
    CMEM.

% Wie beschrieben soll die neue NNr für den Client in der CMEM gesavet werden.
% Hierfür muss man ggf. durch die ganze CMEM Liste laufen.
% Wird der Client nicht gefunden wird ein kommplett neues Tupel erstellt und hinten angefügt.
updateClient({ErinnerungsZeitSek, TupelListe}, ClientPid, NNr, LogDatei) ->
    NeueTupelListe = setClientNNr(TupelListe, ClientPid, NNr),
    logge_status(io_lib:format("~p update zu NNR: ~p", [ClientPid, NNr]), LogDatei),
    CMEM = {ErinnerungsZeitSek, NeueTupelListe},
    CMEM.

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
% Wie beschrieben muss dafür ggf. die ganze CMEM Liste durch gegangen werden.
% Wird der Client nicht gefunden, dann wird die Initial NNr (Default (=0) + 1) ausgegeben.
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

% Hier wird die CMEM "terminiert".
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
