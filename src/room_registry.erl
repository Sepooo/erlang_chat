-module(room_registry).
-export([start_link/0, create_room/3, request_join_room/2, broadcast/3, list_rooms/0, quit_room/2, close_room/2, get_room_pid/1, get_owner/1]).

-define(TABLE, room_registry).

start_link() ->
    ets:new(?TABLE, [named_table, public, set]),
    {ok, self()}.

create_room(RoomName, Creator, IsPrivate) ->
    case ets:lookup(?TABLE, RoomName) of
        [] ->
            {ok, Pid} = room:start_link(RoomName, Creator, IsPrivate),
            ets:insert(?TABLE, {RoomName, Pid}),
            ok;
        _ ->
            {error, already_exists}
    end.

request_join_room(RoomName, Nickname) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {request_join, Nickname, self()},
            receive
                {join_ok} ->
                    ok;
                {join_denied} ->
                    {error, not_invited}
            after 1000 ->
                {error, timeout}
            end;
        [] ->
            {error, room_not_found}
    end.

broadcast(RoomName, From, Message) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {broadcast, From, Message},
            ok;
        [] ->
            {error, room_not_found}
    end.

% {RoomName, Pid, IsPrivate, InvitedList}
list_rooms() ->
    lists:map(fun({RoomName, Pid}) ->
        Pid ! {get_room_info, self()},
        receive
            {room_info, RoomName, IsPrivate, InvitedList} ->
                {RoomName, IsPrivate, InvitedList}
        after 500 ->
            {RoomName, error, []}
        end
    end, ets:tab2list(?TABLE)).



quit_room(RoomName, Nickname) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {quit, Nickname},
            ok;
        [] ->
            {error, room_not_found}
    end.

close_room(RoomName, Nickname) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {close, Nickname},
            ets:delete(?TABLE, RoomName),
            ok;
        [] ->
            {error, room_not_found}
    end.

get_room_pid(RoomName) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

get_owner(RoomName) ->
    error_logger:info_msg("Getting owner for room: ~p~n", [RoomName]),
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {get_owner, self()},
            receive
                {owner, Owner} -> {ok, Owner}
            after 1000 ->
                {error, timeout}
            end;
        [] ->
            {error, not_found}
    end.
