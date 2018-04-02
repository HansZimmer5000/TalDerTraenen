-module(dlq).

% API
-export([
	initDLQ/2, 
	delDLQ/1, 

	expectedNr/1, 
	hole_max_nnr/1,

	push2DLQ/3, 
	entferne_letztes_listen_element/1,
	dlq_ist_voll/1,

	deliverMSG/4,
	pruefe_nnr_und_hole_nachricht/3,
	hole_naechst_groessere_nnr/2,
	hole_nachricht/2,
	erstelle_err_nachricht/0
	]).

-define(DLQ_EMPTY_NNR, 0).

%------------------------------------------------------------------------------------------------------
%										>>SCHNITTSTELLEN<<
%------------------------------------------------------------------------------------------------------
% Initialisiert die DLQ
initDLQ(Size,Datei) ->
  logge_status(io_lib:format("Neue DLQ mit Size ~p",[Size]), Datei),
  {Size, []}.

% Loeschen der DLQ
delDLQ(_DLQ) -> ok.

% Gibt die Nachrichtennummer zurueck die als naechstes erwartet wird. (Die letzte / groeßte Nachrichtennummer + 1)
expectedNr({_Size, Messages}) ->
	MaxNr = hole_max_nnr(Messages),
	MaxNr + 1.

hole_max_nnr([]) ->
	?DLQ_EMPTY_NNR;
hole_max_nnr([AktuellsteNachricht | _RestlicheNachrichten]) ->
	[NNr, _Text, _TSCOut, _TSHIn ,_TSDIn] = AktuellsteNachricht,
	NNr.


% Fuegt eine neue Nachricht in die DLQ ein.
% Da absteigend sortiert ist heißt das ganz vorne.
% Vorausgesetzt die DLQ (Size) ist noch nicht voll! Wenn voll wird neue Nachricht einfach verworfen und Fehler gelogt.
push2DLQ([NNr, Msg, TSClientOut, TSHBQin], {Size, Nachrichten}, Datei) ->
	DLQIstVoll = dlq_ist_voll({Size, Nachrichten}),
	case DLQIstVoll of
		true ->
			logge_status("DLQ ist voll, letzte Nachricht wird verworfen", Datei),
			TmpNeueNachrichten = entferne_letztes_listen_element(Nachrichten);
		false ->
			TmpNeueNachrichten = Nachrichten
	end,
	TSDLQIn = erlang:timestamp(),
	NeueNachrichten = [[NNr, Msg, TSClientOut, TSHBQin, TSDLQIn] | TmpNeueNachrichten],
	logge_status("Neue Nachricht wurde vorne angefuegt", Datei),
	NeueDLQ = {Size, NeueNachrichten},
	NeueDLQ.

% Prueft ob die DLQ schon voll ist.
dlq_ist_voll({Size, Nachrichten}) ->
	Size == length(Nachrichten).

entferne_letztes_listen_element([]) -> [];
entferne_letztes_listen_element(Nachrichten) ->
	lists:droplast(Nachrichten).

% Sendet eine Bestimmte Nachricht (anhand NNr) and bestimmten Client (ClientPID), gibt die gesendete Nummer zurueck.
deliverMSG(NNr, ClientPID, {_Size, DLQNachrichten}, Datei) ->
	case pruefe_nnr_und_hole_nachricht(DLQNachrichten, NNr, Datei) of
		[] ->
			logge_status(io_lib:format("Nachricht mit Nummer ~p nicht existent",[NNr]), Datei),
			ZuSendendeNachricht = erstelle_err_nachricht(),
			TerminatedFlag = true;
		GefundeneNachricht ->	
			[GefundeneNNr | _] = GefundeneNachricht,
			logge_status(io_lib:format("Nachricht mit Nummer ~p existent", [GefundeneNNr]), Datei),
			ZuSendendeNachricht = GefundeneNachricht,
			TerminatedFlag = (pruefe_nnr_und_hole_nachricht(DLQNachrichten, GefundeneNNr + 1, Datei) == [])
	end,

	GesendeteNachrichtMitTS = fuege_dlqout_ts_hinzu(ZuSendendeNachricht),
	ClientPID ! {reply, GesendeteNachrichtMitTS, TerminatedFlag},

	[GesendeteNNr | _] = GesendeteNachrichtMitTS,
	GesendeteNNr.

% Holt anhand der Nachrichtennummer eine Nachricht (oder die naechst groessere) aus einer Liste von Messages.
% [] wird zurueckgegeben wenn keine passende Nachricht gefunden werden konnte.
pruefe_nnr_und_hole_nachricht(DLQNachrichten, GesuchteNNr, Datei) ->
	case hole_nachricht(DLQNachrichten, GesuchteNNr) of
		[] ->
			DLQNachrichtenReversed = lists:reverse(DLQNachrichten),
			case hole_naechst_groessere_nnr(DLQNachrichtenReversed, GesuchteNNr) of
				GesuchteNNr ->
					[];
				NaechstGroessereNNr ->
					logge_status(io_lib:format("Naechst Groessere NNr = ~p", [NaechstGroessereNNr]), Datei),
					hole_nachricht(DLQNachrichten, NaechstGroessereNNr)
			end;
		GefundeneNachricht ->
			GefundeneNachricht
	end.

hole_nachricht([], _NNr) -> 
	[];
hole_nachricht([[NNr | NachrichtRest] | _RestlicheNachrichten], NNr) -> 
	[NNr | NachrichtRest];
hole_nachricht([_AktuellsteNachricht | RestlicheNachrichten], NNr) -> 
	hole_nachricht(RestlicheNachrichten, NNr).

% hole_naechst_groessere_nnr rechnet mit einer Aufsteigend sortierten Liste!
hole_naechst_groessere_nnr([], AusgangsNNr) ->
	AusgangsNNr;
hole_naechst_groessere_nnr([DLQKopfNachricht | DLQRestNachrichten], AusgangsNNr) ->
	[DLQKopfNNr | _Rest] = DLQKopfNachricht,
	case DLQKopfNNr > AusgangsNNr of
		true ->
			DLQKopfNNr;
		false ->
			hole_naechst_groessere_nnr(DLQRestNachrichten, AusgangsNNr)
	end.

% Erstellt eine Error Nachricht aufgrund des nicht vorhanden seins der gesuchten Nachrichtennummer in der DLQ.
erstelle_err_nachricht() ->
	TS = erlang:timestamp(),
	[?DLQ_EMPTY_NNR, "Angeforderte Nachricht nicht vorhanden.", TS, TS, TS].

fuege_dlqout_ts_hinzu([NNr, Text, TSClientOut, TSHBQin, TSDLQIn]) ->
	TSDLQOut = erlang:timestamp(),
	[NNr, Text, TSClientOut, TSHBQin, TSDLQIn, TSDLQOut].

%------------------------------------------------------------------------------------------------------
%											>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

logge_status(Inhalt, LogDatei) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(LogDatei, LogNachricht).
