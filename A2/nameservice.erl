-module(nameservice).

-export([
    start/0,

    bind/3,
    rebind/3,
    lookup/2,
    unbind/2,
    multicastvote/2,
    send_vote_to_all_ggtprocesses/3,
    reset/0
]).

-define(NSNAME, nameservice).
-define(LOG_DATEI_NAME, "nameservice.log").

start() ->
    logge_status("nameservice gestartet"),
    register(?NSNAME, self()),
    receive_loop([]).

receive_loop(NamesToPids) ->
    receive
        {AbsenderPid, {bind, Name, Node}} ->    logge_status("got bind"),
                                                {NewNamesToPids, ResultMessage} = bind(NamesToPids, Name, {Name, Node}),
                                                sendMessage(AbsenderPid, ResultMessage),
                                                receive_loop(NewNamesToPids);
        {AbsenderPid, {rebind, Name, Node}} ->  logge_status("got rebind"),
                                                {NewNamesToPids, ResultMessage} = rebind(NamesToPids, Name, {Name, Node}),
                                                sendMessage(AbsenderPid, ResultMessage),
                                                receive_loop(NewNamesToPids);
        {AbsenderPid, {lookup, Name}} ->    logge_status("got lookup"),
                                            ResultMessage = lookup(NamesToPids, Name),
                                            sendMessage(AbsenderPid, ResultMessage),
                                            receive_loop(NamesToPids);
        {AbsenderPid, {unbind, Name}} ->    logge_status("got unbind"),
                                            {NewNamesToPids, ResultMessage} = unbind(NamesToPids, Name),
                                            sendMessage(AbsenderPid, ResultMessage),
                                            receive_loop(NewNamesToPids);
        {_AbsenderPid, {multicast, vote, InitatorName}} ->  logge_status("got multicast vote"),
                                                            multicastvote(NamesToPids, InitatorName),
                                                            receive_loop(NamesToPids);
        {AbsenderPid, reset} -> logge_status("got reset"),
                                {NewNamesToPids, ResultMessage} = reset(),
                                sendMessage(AbsenderPid, ResultMessage),
                                receive_loop(NewNamesToPids)
    end.

bind(NamesToPids, NewName, NewPid) ->
    NewNamesToPids = bind_(NamesToPids, NewName, NewPid, []),
    case NewNamesToPids of
        NamesToPids -> ResultMessage = in_use;
        _Any -> ResultMessage = ok
    end,
    {NewNamesToPids, ResultMessage}.

bind_([], NewName, NewPid, Akku) -> lists:append([{NewName, NewPid}], Akku);
bind_([HeadTupel | RestTupels], NewName, NewPid, Akku) ->
    {HeadName, _HeadPid} = HeadTupel,
    case HeadName of
        NewName ->  NewNamesToPids = lists:append([Akku, [HeadTupel], RestTupels]),
                    NewNamesToPids;
        _Any -> NewAkku = lists:append(Akku, [HeadTupel]),
                bind_(RestTupels, NewName, NewPid, NewAkku)
    end.

rebind(NamesToPids, NewName, NewPid) ->
    {NewNamesToPids1, ok} = unbind(NamesToPids, NewName),
    {NewNamesToPids2, ok} = bind(NewNamesToPids1, NewName, NewPid),
    {NewNamesToPids2, ok}.

unbind(NamesToPids, Name) ->
    NewNamesToPids = unbind_(NamesToPids, Name, []),
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

unbind_([], _Name, Akku) -> Akku;
unbind_([HeadTupel | RestTupels], Name, Akku) ->
    {HeadName, _HeadPid} = HeadTupel,
    case HeadName of
        Name -> NewNamesToPids = lists:append(Akku, RestTupels),
                NewNamesToPids;
        _Any -> NewAkku = lists:append(Akku, [HeadTupel]),
                unbind_(RestTupels, Name, NewAkku)
    end.

lookup([], _Name) -> not_found;
lookup([HeadTupel | RestTupels], Name) ->
    {HeadName, HeadPid} = HeadTupel,
    case HeadName of
        Name -> {pin, HeadPid};
        _Any -> lookup(RestTupels, Name)
    end.

multicastvote(NamesToPids, InitatorName) ->
    case lookup(NamesToPids, InitatorName) of
        {pin, InitatorPid} -> send_vote_to_all_ggtprocesses(NamesToPids, InitatorPid, InitatorName);
        not_found -> logge_status(lists:flatten(io_lib:format("vote Initiator ~p nicht bekannt", [InitatorName])))
    end.

send_vote_to_all_ggtprocesses([], _InitatorPid, _InitatorName) -> done;
send_vote_to_all_ggtprocesses([HeadTupel | RestTupels], InitatorPid, InitatorName) ->
    {HeadName, HeadPid} = HeadTupel,
    case HeadName of
        koordinator -> donothing;
        InitatorName -> donothing;
        _Any -> HeadPid ! {InitatorPid, {vote, InitatorName}}
    end,
    send_vote_to_all_ggtprocesses(RestTupels, InitatorPid, InitatorName).

reset() ->
    NewNamesToPids = [],
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

sendMessage(Receiver, Message) ->
    Receiver ! Message.



logge_status(Inhalt) ->
    AktuelleZeit = vsutil:now2string(erlang:timestamp()),
    LogNachricht = io_lib:format("~p ~s.\n", [AktuelleZeit, Inhalt]),
    io:fwrite(LogNachricht),
    util:logging(?LOG_DATEI_NAME, LogNachricht).