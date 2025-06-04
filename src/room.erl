-module(room).
-export([start_link/2]).

-record(state, {
    name,
    creator,
    users = #{}
}).

start_link(RoomName, Creator) ->
    Pid = spawn_link(fun() -> loop(#state{name=RoomName, creator=Creator}) end),
    {ok, Pid}.

loop(State = #state{name=RoomName, creator=Creator, users=Users}) ->
    receive
        {join, Nick, ClientPid} ->
            NewUsers = Users#{Nick => ClientPid},
            ClientPid ! {info, <<"You joined room: ", RoomName/binary, "\r\n">>},
            loop(State#state{users = NewUsers});

        {broadcast, From, Msg} ->
            lists:foreach(
                fun({_Nick, Pid}) ->
                    Pid ! {message, RoomName, From, Msg}
                end,
                maps:to_list(Users)
            ),
            loop(State);

        {quit, Nick} ->
            NewUsers = maps:remove(Nick, Users),
            loop(State#state{users = NewUsers});

        {close, Nick} when Nick =:= Creator ->
            lists:foreach(
                fun({_Nick, Pid}) ->
                    Pid ! {info, <<"Room closed by owner.">>}
                end,
                maps:to_list(Users)
            ),
            exit(normal);

        _ ->
            loop(State)
    end.
