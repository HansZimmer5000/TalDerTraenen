%%%-------------------------------------------------------------------
%%% @author Arne Thiele & Michael Müller
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2017 13:30
%%%-------------------------------------------------------------------
-module(dlq).
-author("Arne Thiele & Michael Müller").

%% API
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

%% CONSTANTS
-define(DLQLOG,'dlq.log').



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
  log_status(initDLQ, io_lib:format("input: Size: ~p, Datei: ~p",[Size,Datei])),
  [Size].

%------------------------------------------------------------------------------------------------------
%										>>SCHNITTSTELLEN UND HANDLER<<
%------------------------------------------------------------------------------------------------------

% Löschen der DLQ
delDLQ(_DLQ) ->
  log_start(delDLQ),
  ok.

% Gibt die Nachrichtennummer zurück die als nächstes erwartet wird. (Die letzte / größte Nachrichtennummer + 1)
expectedNr([_Size|Messages]) ->
	log_start(expectedNr),
	MaxNr = getMaxNr(Messages),
	log_status(expectedNr,io_lib:format("expectedNr: ~p",[MaxNr + 1])),
	MaxNr + 1.

% Gibt die höchste Nachrichtennummer heraus.
% Wäre auch mit length(List) möglich
getMaxNr([]) ->
	log_status(getMaxNr,"input: Leere Messages"),
	0;
getMaxNr([[MaxNr|_HeadRest]|_MessagesRest]) ->
	log_status(getMaxNr,io_lib:format("input: MaxNr: ~p",[MaxNr])),
	MaxNr.


% Fügt eine neue Nachricht in die DLQ ein.
% Da absteigend sortiert ist heißt das ganz vorne.
% Vorausgesetzt die DLQ (Size) ist noch nicht voll! Wenn voll wird neue Nachricht einfach verworfen und Fehler gelogt.
push2DLQ([NNr,Msg,TSClientOut,TSHBQin],[Size|Messages],_Datei) ->
	log_status(push2DLQ,io_lib:format("input: NNR: ~p",[NNr])),
	DLQIsFilled = isDLQFilled([Size|Messages]),
	log_status(push2DLQ,io_lib:format("DLQIsFilled: ~p",[DLQIsFilled])),
	if
		DLQIsFilled ->
			log_status(push2DLQ,"DLQ ist voll, älteste Nachricht wird verworfen."),
			TmpMessages = throwOldestOut(Messages);
			%NewDLQ = [Size|NewMessages],
			%log_status(push2DLQ,io_lib:format("DLQ ist voll, neue Nachricht (NNr: ~p) wird nicht eingefügt sondern verworfen!",[NNr]));
		true ->
			TmpMessages = Messages
	end,
	TSDLQIn = erlang:timestamp(),
	NewMessages = [[NNr,Msg,TSClientOut,TSHBQin,TSDLQIn]|TmpMessages],
	log_status(push2DLQ,"Neue Nachricht wurde vorne angefügt und mit TimeStamp für DLQIn versehen"),
	NewDLQ = [Size|NewMessages],
	NewDLQ.

% Sendet eine Bestimmte Nachricht (anhand NNr) and bestimmten Client (ClientPID), gibt die gesendete Nummer zurück.
deliverMSG(NNr,ClientPID,[_Size|Messages],_Datei) ->
  log_status(deliverMSG,io_lib:format("input: NNr: ~p ClientPID: ~p",[NNr, ClientPID])),
  % Nachricht holen & Existent -> Wenn nicht Fehler Nachricht erzeugen.
  ReturnedMessage = getMessage(Messages,NNr),
  if
    ReturnedMessage == [] ->
      log_status(deliverMSG, io_lib:format("Nachrichtnr: ~p nicht existent, Fehlernachricht wird erzeugt",[NNr])),
      [ErrNNr,ErrMsg|ErrRest] = createErrMSGMissingNNr(),
      Message = [ErrNNr,ErrMsg|ErrRest],
      Termi = true;
      %OutgoingNNr = ErrNNr;
    true ->
      [FoundNNr|_FoundRestMsg] = ReturnedMessage,	
      log_status(deliverMSG, io_lib:format("Nachrichtnr: ~p existent",[FoundNNr])),
      Message = ReturnedMessage,
      Termi = getMessage(Messages,NNr + 1) == []
      %OutgoingNNr = NNr
  end,
  % Nachricht TSDLQOut anfügen.
  log_status(deliverMSG,io_lib:format("Message about to go out: ~p",[Message])),
  [OutgoingNNr, Msg, TSClientOut, TSHBQin, TSDLQIn] = Message,
  OutgoingMessage = [OutgoingNNr, Msg, TSClientOut, TSHBQin, TSDLQIn, erlang:timestamp()],
  % Nachricht versenden.
  ClientPID ! {reply,OutgoingMessage,Termi},
  OutgoingNNr.


%------------------------------------------------------------------------------------------------------
%											>>HILFSMETHODEN<<
%------------------------------------------------------------------------------------------------------

% Schmeisst das letzte (aelteste) Element raus
throwOldestOut(Messages) ->
	log_start(throwOldestOut),
	%lists:reverse(tl(lists:reverse(Messages))).
	lists:droplast(Messages).


% Prüft ob die DLQ schon voll ist, also ob die Size schon erreicht wurde.
isDLQFilled([Size|Messages]) ->
	log_start(isDLQFilled),
	if
		Size == length(Messages) ->
			DLQIsFilled = true;
		true ->
			DLQIsFilled = false
	end,
	DLQIsFilled.

% Holt anhand der Nachrichtennummer eine Nachrichte aus eine Liste von Messages.
% [] wird zurückgegeben wenn die Nachricht nicht gefunden werden konnte.
getMessage([],NNr) -> 
	log_status(getMessage,io_lib:format("NNr: ~p nicht gefunden",[NNr])),
	[];
getMessage([[LastElemNNr|LastElemRest]],1) ->
	log_status(getMessage,io_lib:format("Nummer 1 angefordert, Nummer ~p aber aktueller Anfang der DLQ",[LastElemNNr])),
	[LastElemNNr|LastElemRest];
getMessage([[NNr|HeadRest]|_MessagesRest], NNr) ->
	log_status(getMessage,io_lib:format("Nachrichtennummer: ~p gefunden",[NNr])),
	[NNr|HeadRest];
getMessage([_Head|MessagesRest], NNr) ->
	getMessage(MessagesRest,NNr).

% Erstellt eine Error Nachricht aufgrund des nicht vorhanden seins der gesuchten Nachrichtennummer in der DLQ.
createErrMSGMissingNNr() ->
	TS = erlang:timestamp(),
	[0,["Angeforderte Nachricht nicht vorhanden."],TS,TS,TS].


%------------------------------------------------------------------------------------------------------
%											>>LOGGING UND CONFIG<<
%------------------------------------------------------------------------------------------------------

% Logt start einer Funktion
log_start(Funktion) -> log_status(Funktion, io_lib:format("~p started. ~n",[Funktion])).

% Logt den aktuellen Status / Wert / ... einer Funktion.
log_status_file(Funktion,Status,Datei) -> werkzeug:logging(Datei, io_lib:format("~p ~p hat status: ~s. ~n",[werkzeug:timeMilliSecond(), Funktion, Status])).
log_status(Funktion,Status) -> log_status_file(Funktion,Status,?DLQLOG).