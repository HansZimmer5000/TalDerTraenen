-module(dlq).
-author("Arne Thiele & Michael Mueller").

% API
-export([
	initDLQ/2, 
	delDLQ/1, 

	expectedNr/1, 
	holeMaxNNr/1,

	push2DLQ/3, 
	entferneLetztesListenElement/1,
	dLQIstVoll/1,

	deliverMSG/4,
	holeNachricht/2,
	erstelleErrNachricht/0
	]).

-define(DLQ_EMPTY_NNR, 0).

%////////////////////////////////
%      DLQ
% Beispiel mit einer Message: [Size,[NNR,["Irgend ein Text"],TSClientOut,TSHBQIn,TSDLQIn,TSDLQOut]]
% DLQ ist so sortiert, dass aktuellste Nachricht (= Hoechste Nachrichtennummer) ganz vorn steht, praktisch absteigend anhand der NNR sortiert.
%////////////////////////////////

%------------------------------------------------------------------------------------------------------
%											>>INIT UND LOOPS<<
%------------------------------------------------------------------------------------------------------

% Initialisiert die DLQ
initDLQ(Size,Datei) ->
  logge_status(io_lib:format("Neue DLQ mit Size ~p",[Size]), Datei),
  [Size, []].

%------------------------------------------------------------------------------------------------------
%										>>SCHNITTSTELLEN<<
%------------------------------------------------------------------------------------------------------

% Loeschen der DLQ
delDLQ(_DLQ) -> ok.

% Gibt die Nachrichtennummer zurueck die als naechstes erwartet wird. (Die letzte / groeßte Nachrichtennummer + 1)
expectedNr([_Size, Messages]) ->
	MaxNr = holeMaxNNr(Messages),
	MaxNr + 1.

holeMaxNNr([]) ->
	?DLQ_EMPTY_NNR;
holeMaxNNr([AktuellsteNachricht | _RestlicheNachrichten]) ->
	[NNr, _Text, _TSCOut, _TSHIn ,_TSDIn] = AktuellsteNachricht,
	NNr.


% Fuegt eine neue Nachricht in die DLQ ein.
% Da absteigend sortiert ist heißt das ganz vorne.
% Vorausgesetzt die DLQ (Size) ist noch nicht voll! Wenn voll wird neue Nachricht einfach verworfen und Fehler gelogt.
push2DLQ([NNr, Msg, TSClientOut, TSHBQin], [Size, Nachrichten], Datei) ->
	DLQIstVoll = dLQIstVoll([Size, Nachrichten]),
	case DLQIstVoll of
		true ->
			logge_status("DLQ ist voll, letzte Nachricht wird verworfen", Datei),
			TmpNeueNachrichten = entferneLetztesListenElement(Nachrichten);
		false ->
			TmpNeueNachrichten = Nachrichten
	end,
	TSDLQIn = erlang:timestamp(),
	NeueNachrichten = [[NNr, Msg, TSClientOut, TSHBQin, TSDLQIn] | TmpNeueNachrichten],
	logge_status("Neue Nachricht wurde vorne angefuegt", Datei),
	NeueDLQ = [Size, NeueNachrichten],
	NeueDLQ.

% Prueft ob die DLQ schon voll ist, also ob die Size schon erreicht wurde.
dLQIstVoll([Size, Nachrichten]) ->
	Size == length(Nachrichten).

entferneLetztesListenElement([]) -> [];
entferneLetztesListenElement(Nachrichten) ->
	lists:droplast(Nachrichten).

% Sendet eine Bestimmte Nachricht (anhand NNr) and bestimmten Client (ClientPID), gibt die gesendete Nummer zurueck.
deliverMSG(NNr, ClientPID, [_Size, Nachrichten], Datei) ->
  GefundeneNachricht = holeNachricht(Nachrichten, NNr),
  case GefundeneNachricht of
	[] ->
		logge_status(io_lib:format("Nachricht mit Nummer ~p nicht existent",[NNr]), Datei),
      	[ErrNNr, ErrText | ErrRest] = erstelleErrNachricht(),
      	Nachricht = [ErrNNr, ErrText | ErrRest],
      	TermiatedFlag = true;
    _ ->
      	[GefundeneNNr | _NachrichtRest] = GefundeneNachricht,	
      	logge_status(io_lib:format("Nachricht mit Nummer ~p existent", [GefundeneNNr]), Datei),
      	Nachricht = GefundeneNachricht,
      	TermiatedFlag = (holeNachricht(Nachrichten, NNr + 1) == [])
  end,

  [ZuSendendeNNr, Text, TSClientOut, TSHBQin, TSDLQIn] = Nachricht,
  ZuSendendeNachricht = [ZuSendendeNNr, Text, TSClientOut, TSHBQin, TSDLQIn, erlang:timestamp()],

  ClientPID ! {reply, ZuSendendeNachricht, TermiatedFlag},
  ZuSendendeNNr.

% Holt anhand der Nachrichtennummer eine Nachrichte aus eine Liste von Messages.
% [] wird zurueckgegeben wenn die Nachricht nicht gefunden werden konnte.
holeNachricht([], _NNr) -> [];
holeNachricht([[NNr | NachrichtRest] | _RestlicheNachrichten], NNr) -> 
	[NNr | NachrichtRest];
holeNachricht([_AktuellsteNachricht | RestlicheNachrichten], NNr) -> 
	holeNachricht(RestlicheNachrichten, NNr).

% Erstellt eine Error Nachricht aufgrund des nicht vorhanden seins der gesuchten Nachrichtennummer in der DLQ.
erstelleErrNachricht() ->
	TS = erlang:timestamp(),
	[?DLQ_EMPTY_NNR, "Angeforderte Nachricht nicht vorhanden.", TS, TS, TS].


%------------------------------------------------------------------------------------------------------
%											>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).

logge_nachricht_status(Nachricht, Status, LogDatei) ->
    [NNR | _Rest] = Nachricht,
    LogNachricht = io_lib:format("NNR ~p ~s", [NNR, Status]),
    logge_status(LogNachricht, LogDatei).
