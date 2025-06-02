-module(chat_room).
-behaviour(gen_server).

-record(state, {
    rooms = #{} % #{room_name => {creator, [members]}
}).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_room(RoomName, Owner) when is_binary(RoomName), is_binary(Owner) ->
    gen_server:call(?MODULE, {create_room, RoomName, Owner}).

join_room(RoomName, Nickname) ->
    gen_server:call(?MODULE, {join_room, RoomName, Nickname}).

quit_room(RoomName, Nickname) ->
    gen_server:cast(?MODULE, {leave_room, RoomName, Nickname}).

destroy_room(RoomName, Creator) ->
    gen_server:call(?MODULE, {destroy_room, RoomName, Owner}).

list_rooms() ->
    gen_server:call(?MODULE, list_rooms).

broadcast(RoomName, From, Message) ->
    gen_server:cast(?MODULE, {broadcast, RoomName, From, MEssage}).

logout(Nickname) ->
    gen_server:cast(?MODULE, {logout, Nickkname}).

init([]) ->
    {ok, #state{}}.
