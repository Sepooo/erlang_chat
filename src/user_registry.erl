%% This module manages an ETS global table to keep track of connected users

-module(user_registry).
-export([start_link/0, register/2, unregister/1, get_pid/1, list_users/0]).

-define(TABLE, user_registry).

start_link() ->
    ets:new(?TABLE, [named_table, public, set]),
    {ok, self()}.

% Registration: check if nickname is already in the TABLE, than adds it to the list
register(Nickname, Pid) ->
    case ets:lookup(?TABLE, Nickname) of
        [] ->
            ets:insert(?TABLE, {Nickname, Pid}),
            ok;
        _ ->
            {error, already_registered}
    end.

unregister(Nickname) ->
    ets:delete(?TABLE, Nickname),
    ok.

get_pid(Nickname) ->
    case ets:lookup(?TABLE, Nickname) of
        [{_, Pid}] -> {ok, Pid};
        [] -> {error, user_not_found}
    end.

list_users() ->
    [Nickname || {Nickname, _Pid} <- ets:tab2list(?TABLE)].