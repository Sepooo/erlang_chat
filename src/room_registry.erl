-module(room_registry).
-export([start_link/0, create_room/3, join_room/2, broadcast/3, list_rooms/0, quit_room/2, close_room/2]).

%% State: ETS o un processo con mappa RoomName -> Pid
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

join_room(RoomName, Nickname) ->
    case ets:lookup(?TABLE, RoomName) of
        [{_, Pid}] ->
            Pid ! {join, Nickname, self()},
            ok;
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

list_rooms() ->
    ets:tab2list(?TABLE).

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
