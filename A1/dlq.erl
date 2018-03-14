-module(dlq).
-author("Arne Thiele & Michael Müller").

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
	erstelleErrNachrichtFurFehlendeNNr/0
	]).


%////////////////////////////////
%      DLQ
% Beispiel mit einer Message: [Size,[NNR,["Message"],TSClientOut,TSHBQIn,TSDLQIn,TSDLQOut]]
% DLQ ist so sortiert, dass aktuellste Nachricht (= Höchste Nachrichtennummer) ganz vorn steht, praktisch absteigend anhand der NNR sortiert.
%////////////////////////////////

%------------------------------------------------------------------------------------------------------
%											>>INIT UND LOOPS<<
%------------------------------------------------------------------------------------------------------

% Initialisiert die DLQ
initDLQ(Size,Datei) ->
  logge_status(io_lib:format("Size: ~p, Datei: ~p",[Size,Datei]), Datei),
  [Size, []].

%------------------------------------------------------------------------------------------------------
%										>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

% Löschen der DLQ
delDLQ(_DLQ) -> ok.

% Gibt die Nachrichtennummer zurück die als nächstes erwartet wird. (Die letzte / größte Nachrichtennummer + 1)
expectedNr([_Size, Messages]) ->
	MaxNr = holeMaxNNr(Messages),
	MaxNr + 1.

% Gibt die höchste Nachrichtennummer heraus.
% Wäre auch mit length(List) möglich
holeMaxNNr([]) ->
	0;
holeMaxNNr([AktuellsteNachricht | _RestlicheNachrichten]) ->
	[NNr, _Text, _TSCOut, _TSHIn ,_TSDIn] = AktuellsteNachricht,
	NNr.


% Fügt eine neue Nachricht in die DLQ ein.
% Da absteigend sortiert ist heißt das ganz vorne.
% Vorausgesetzt die DLQ (Size) ist noch nicht voll! Wenn voll wird neue Nachricht einfach verworfen und Fehler gelogt.
push2DLQ([NNr, Msg, TSClientOut, TSHBQin], [Size, Nachrichten], Datei) ->
	DLQIstVoll = dLQIstVoll([Size, Nachrichten]),
	logge_status(io_lib:format("DLQIstVoll: ~p", [DLQIstVoll]), Datei),
	if
		DLQIstVoll ->
			logge_status("DLQ ist voll, älteste Nachricht wird verworfen", Datei),
			TmpNeueNachrichten = entferneLetztesListenElement(Nachrichten);
		true ->
			TmpNeueNachrichten = Nachrichten
	end,
	TSDLQIn = erlang:timestamp(),
	NeueNachrichten = [[NNr, Msg, TSClientOut, TSHBQin, TSDLQIn] | TmpNeueNachrichten],
	logge_status("Neue Nachricht wurde vorne angefügt und mit Timestamp für DLQIn versehen", Datei),
	NeueDLQ = [Size, NeueNachrichten],
	NeueDLQ.

% Sendet eine Bestimmte Nachricht (anhand NNr) and bestimmten Client (ClientPID), gibt die gesendete Nummer zurück.
deliverMSG(NNr, ClientPID,[_Size, Nachrichten], Datei) ->
  logge_status(io_lib:format("input: NNr: ~p ClientPID: ~p",[NNr, ClientPID]), Datei),
  % Nachricht holen & Existent -> Wenn nicht Fehler Nachricht erzeugen.
  GefundeneNachricht = holeNachricht(Nachrichten, NNr),
  if
    GefundeneNachricht == [] ->
      logge_status(io_lib:format("Nachrichtnr: ~p nicht existent, Fehlernachricht wird erzeugt",[NNr]), Datei),
      [ErrNNr, ErrText | ErrRest] = erstelleErrNachrichtFurFehlendeNNr(),
      Nachricht = [ErrNNr, ErrText | ErrRest],
      TermiatedFlag = true;
    true ->
      [GefundeneNNr | _NachrichtRest] = GefundeneNachricht,	
      logge_status(io_lib:format("Nachrichtnr: ~p existent", [GefundeneNNr]), Datei),
      Nachricht = GefundeneNachricht,
      TermiatedFlag = holeNachricht(Nachrichten, NNr + 1) == []
  end,
  % Nachricht TSDLQOut anfügen.
  logge_nachricht_status(Nachricht, "zu verschicken", Datei),
  [ZuSendendeNNr, Text, TSClientOut, TSHBQin, TSDLQIn] = Nachricht,
  ZuSendendeNachricht = [ZuSendendeNNr, Text, TSClientOut, TSHBQin, TSDLQIn, erlang:timestamp()],
  % Nachricht versenden.
  ClientPID ! {reply, ZuSendendeNachricht, TermiatedFlag},
  ZuSendendeNNr.


%------------------------------------------------------------------------------------------------------
%											>>HILFSMETHODEN<<
%------------------------------------------------------------------------------------------------------

% Schmeisst das letzte (aelteste) Element raus
entferneLetztesListenElement([]) -> [];
entferneLetztesListenElement(Nachrichten) ->
	%lists:reverse(tl(lists:reverse(Messages))).
	lists:droplast(Nachrichten).


% Prüft ob die DLQ schon voll ist, also ob die Size schon erreicht wurde.
dLQIstVoll([Size, Nachrichten]) ->
	if
		Size == length(Nachrichten) ->
			DLQIstVoll = true;
		true ->
			DLQIstVoll = false
	end,
	DLQIstVoll.

% Holt anhand der Nachrichtennummer eine Nachrichte aus eine Liste von Messages.
% [] wird zurückgegeben wenn die Nachricht nicht gefunden werden konnte.
holeNachricht([], _NNr) -> [];
holeNachricht([[NNr | NachrichtRest] | _RestlicheNachrichten], NNr) -> 
	[NNr | NachrichtRest];
holeNachricht([_AktuellsteNachricht | RestlicheNachrichten], NNr) -> 
	holeNachricht(RestlicheNachrichten, NNr).

% Erstellt eine Error Nachricht aufgrund des nicht vorhanden seins der gesuchten Nachrichtennummer in der DLQ.
erstelleErrNachrichtFurFehlendeNNr() ->
	TS = erlang:timestamp(),
	[0, "Angeforderte Nachricht nicht vorhanden.", TS, TS, TS].


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
