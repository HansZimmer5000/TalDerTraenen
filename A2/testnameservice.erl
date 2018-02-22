-module(testnameservice).

-include_lib("eunit/include/eunit.hrl").

%    bind/4,
%    rebind/4,
%    unbind/3,
%    lookup/3,
%    multicastvote/2,
%    reset/1

bind_1_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    NewName = nameB,
    NewPid = {nameB, nodeB},
    {NewNamesToPids, ResultMessage} = nameservice:bind(NamesToPids, NewName, NewPid),
    ?assertEqual([{NewName, NewPid}, {nameA, {nameA, nodeA}}], NewNamesToPids),
    ?assertEqual(ok, ResultMessage).

bind_2_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    NewName = nameA,
    NewPid = {nameA, nodeA},
    {NewNamesToPids, ResultMessage} = nameservice:bind(NamesToPids, NewName, NewPid),
    ?assertEqual([{nameA, {nameA, nodeA}}], NewNamesToPids),
    ?assertEqual(in_use, ResultMessage).

rebind_1_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    NewName = nameB,
    NewPid = {nameB, nodeB},
    {NewNamesToPids, ResultMessage} = nameservice:bind(NamesToPids, NewName, NewPid),
    ?assertEqual([{NewName, NewPid}, {nameA, {nameA, nodeA}}], NewNamesToPids),
    ?assertEqual(ok, ResultMessage).

rebind_2_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    NewName = nameA,
    NewPid = {nameANew, nodeANew},
    {NewNamesToPids, ResultMessage} = nameservice:rebind(NamesToPids, NewName, NewPid),
    ?assertEqual([{NewName, NewPid}], NewNamesToPids),
    ?assertEqual(ok, ResultMessage).

unbind_1_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    {NewNamesToPids, ResultMessage} = nameservice:unbind(NamesToPids, nameA),
    ?assertEqual([], NewNamesToPids),
    ?assertEqual(ok, ResultMessage).

lookup_1_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    ResultMessage = nameservice:lookup(NamesToPids, nameA),
    ?assertEqual({pin, {nameA, nodeA}}, ResultMessage).

lookup_2_test() ->
    NamesToPids = [{nameA, {nameA, nodeA}}],
    ResultMessage = nameservice:lookup(NamesToPids, nameB),
    ?assertEqual(not_found, ResultMessage).


multicastvote_1_test() ->
    throw("Not implemented yet").

reset_1_test() ->
    {NewNamesToPids, ResultMessage} = nameservice:reset(),
    ?assertEqual(ok, ResultMessage),
    ?assertEqual([], NewNamesToPids).


