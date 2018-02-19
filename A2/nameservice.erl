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
    NewNamesToPids = NamesToPids,
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

rebind(NamesToPids, NewName, NewPid) ->
    NewNamesToPids = NamesToPids,
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

unbind(NamesToPids, Name) ->
    NewNamesToPids = NamesToPids,
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

lookup(NamesToPids, Name) ->
    ResultMessage = not_found,
    ResultMessage.

multicastvote(NewNamesToPids, InitatorName) ->
    ok.
    %{AbsenderPid, {vote, MeinName}}.

reset() ->
    NewNamesToPids = [],
    ResultMessage = ok,
    {NewNamesToPids, ResultMessage}.

sendMessage(Receiver, Message) ->
    Receiver ! Message.