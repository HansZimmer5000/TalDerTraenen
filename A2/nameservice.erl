-module(nameservice).

-export([
    start/0,

    bind/3,
    rebind/3,
    lookup/2,
    unbind/2,
    multicastvote/2,
    reset/0
]).

-define(NSNAME, nameservice).

start() ->
    register(?NSNAME, self()),
    receive_loop([]).

receive_loop(NamesToPids) ->
    receive
        {AbsenderPid, {bind, Name, Node}} ->    {NewNamesToPids, ResultMessage} = bind(NamesToPids, Name, {Name, Node}),
                                                sendMessage(AbsenderPid, ResultMessage),
                                                receive_loop(NewNamesToPids);
        {AbsenderPid, {rebind, Name, Node}} ->  {NewNamesToPids, ResultMessage} = rebind(NamesToPids, Name, {Name, Node}),
                                                sendMessage(AbsenderPid, ResultMessage),
                                                receive_loop(NewNamesToPids);
        {AbsenderPid, {lookup, Name}} ->    ResultMessage = lookup(NamesToPids, Name),
                                            sendMessage(AbsenderPid, ResultMessage),
                                            receive_loop(NamesToPids);
        {AbsenderPid, {unbind, Name}} ->    {NewNamesToPids, ResultMessage} = unbind(NamesToPids, Name),
                                            sendMessage(AbsenderPid, ResultMessage),
                                            receive_loop(NewNamesToPids);
        {_AbsenderPid, {multicast, vote, InitatorName}} ->  multicastvote(NamesToPids, InitatorName),
                                                            receive_loop(NamesToPids);
        {AbsenderPid, reset} -> {NewNamesToPids, ResultMessage} = reset(),
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

multicastvote(_NewNamesToPids, _InitatorName) ->
    ok.
    %{AbsenderPid, {vote, MeinName}}.

reset() ->
    NewNamesToPids = [],
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

sendMessage(Receiver, Message) ->
    Receiver ! Message.