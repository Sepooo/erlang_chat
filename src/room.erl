-module(room).
-export([start_link/3]).

-record(state, {
    name,                   %% Room name
    owner,                  %% Name of room's owner
    users = #{},            %% List of users inside the room
    is_private = false,     %% isPrivate flag
    invited = []            %% List if invited users
}).

start_link(RoomName, Owner, IsPrivate) ->
    Pid = spawn_link(fun() -> loop(#state{name=RoomName, owner=Owner, is_private=IsPrivate}) end),
    {ok, Pid}.

loop(State = #state{name=RoomName, owner=Owner, users=Users, invited=Invited}) ->
    receive
        % Sends inviation to user
        {invite, From, ToPid, ToNickname} when From =:= Owner ->
            ToPid ! {invitation, RoomName, From},
            loop(State#state{invited = [ToNickname | Invited]});

        % Accept invitation 
        {accept_invite, Nickname, FromPid} ->
            case lists:member(Nickname, State#state.invited) of
                true -> % if user is inside invited list
                    NewUsers = Users#{Nickname => FromPid},
                    FromPid ! {join_ok},
                    loop(State#state{users = NewUsers});
                false -> % else
                    FromPid ! {join_denied},
                    loop(State)
            end;

        % Request to join a room
        {request_join, Nickname, FromPid} ->
            % check if user can access room
            CanJoin =
                case State#state.is_private of
                    true -> lists:member(Nickname, [State#state.owner | State#state.invited]);
                    false -> true
                end,

            case CanJoin of
                true ->
                    % User is inside invited list or is the owner
                    NewUsers = Users#{Nickname => FromPid},
                    FromPid ! {join_ok},
                    loop(State#state{users = NewUsers});
                false ->
                    % User cannot access
                    FromPid ! {join_denied},
                    loop(State)
            end;

        % Send message into the chat
        {broadcast, From, Msg} ->
            % send message to every user in the room
            lists:foreach(
                fun({_Nickname, Pid}) ->
                    Pid ! {message, RoomName, From, Msg}
                end,
                maps:to_list(Users)
            ),
            loop(State);

        % Quit room
        {quit, Nickname} ->
            NewUsers = maps:remove(Nickname, Users),
            loop(State#state{users = NewUsers});

        % Close room (only for owner)
        {close, Nickname} when Nickname =:= Owner ->
            lists:foreach(
                fun({_Nickname, Pid}) ->
                    Pid ! {info, <<"Room closed by owner.">>}
                end,
                maps:to_list(Users)
            ),
            exit(normal);

        % Get owner
        {get_owner, From} ->
            From ! {owner, Owner},
            loop(State);

        % Get room info (name, isPrivate, Invited list + owner)
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
