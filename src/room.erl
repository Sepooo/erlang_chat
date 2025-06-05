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

        {accept_invite, Nickname, FromPid} ->
            case lists:member(Nickname, State#state.invited) of
                true ->
                    NewUsers = Users#{Nickname => FromPid},
                    FromPid ! {join_ok},
                    loop(State#state{users = NewUsers});
                false ->
                    FromPid ! {join_denied},
                    loop(State)
            end;

        {request_join, Nickname, FromPid} ->
            CanJoin =
                case State#state.is_private of
                    true -> lists:member(Nickname, [State#state.owner | State#state.invited]);
                    false -> true
                end,

            case CanJoin of
                true ->
                    NewUsers = Users#{Nickname => FromPid},
                    FromPid ! {join_ok},
                    loop(State#state{users = NewUsers});
                false ->
                    FromPid ! {join_denied},
                    loop(State)
            end;

        {broadcast, From, Msg} ->
            lists:foreach(
                fun({_Nickname, Pid}) ->
                    Pid ! {message, RoomName, From, Msg}
                end,
                maps:to_list(Users)
            ),
            loop(State);

        {quit, Nickname} ->
            NewUsers = maps:remove(Nickname, Users),
            loop(State#state{users = NewUsers});

        {close, Nickname} when Nickname =:= Owner ->
            lists:foreach(
                fun({_Nickname, Pid}) ->
                    Pid ! {info, <<"Room closed by Owner.">>}
                end,
                maps:to_list(Users)
            ),
            exit(normal);

        {get_owner, From} ->
            From ! {owner, Owner},
            loop(State);

        {get_room_info, From} ->
            InvitedWithOwner =
                case State#state.is_private of
                    true  -> lists:usort([State#state.owner | State#state.invited]);
                    false -> []
                end,
            From ! {room_info, State#state.name, State#state.is_private, InvitedWithOwner},
            loop(State);

        _ ->
            loop(State)
    end.
