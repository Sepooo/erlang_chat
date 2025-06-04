-module(room).
-export([start_link/3]).

-record(state, {
    name,
    owner,
    users = #{},
    is_private = false,
    invited = []
}).

start_link(RoomName, Owner, IsPrivate) ->
    Pid = spawn_link(fun() -> loop(#state{name=RoomName, owner=Owner, is_private=IsPrivate}) end),
    {ok, Pid}.

loop(State = #state{name=RoomName, owner=Owner, users=Users, invited=Invited}) ->
    receive
        {invite, From, ToPid, ToNickname} when From =:= Owner ->
            ToPid ! {invitation, RoomName, From},
            loop(State#state{invited = [ToNickname | Invited]});

        {accept_invite, Nickname, ClientPid} ->
            case lists:member(Nickname, State#state.invited) of
                true ->
                    NewUsers = Users#{Nickname => ClientPid},
                    ClientPid ! {info, <<"You can now access private room: ", RoomName/binary, "\r\n">>},
                    loop(State#state{users = NewUsers});
                false ->
                    ClientPid ! {info, <<"You are not invited to this room.\r\n">>},
                    loop(State)
            end;

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

        {close, Nick} when Nick =:= Owner ->
            lists:foreach(
                fun({_Nick, Pid}) ->
                    Pid ! {info, <<"Room closed by Owner.">>}
                end,
                maps:to_list(Users)
            ),
            exit(normal);

        {get_owner, From} ->
            From ! {owner, Owner},
            loop(State);

        _ ->
            loop(State)
    end.
