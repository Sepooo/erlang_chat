-module(chat_client_handler).

-export([start/1]).

start(Socket) ->
    inet:setopts(Socket, [{active, false}, binary]),
    gen_tcp:send(Socket, <<"Please, enter your nickname: ">>),
    nickname_loop(Socket).

%%%%%%%%
%%% 
%%%  REGISTRATION LOOP 
%%%  Handles the user input to registrate the client as a new user
%%%
%%%%%%%% 
nickname_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            case parse_nickname(Data) of
                {ok, NewNick} ->
                    case user_registry:register(NewNick, self()) of % Registration
                        ok -> 
                            gen_tcp:send(Socket, <<"Welcome ", NewNick/binary, "!\nType /help for commands\r\n">>),
                            start_hub(Socket, NewNick);
                        {error, already_registered} ->
                            gen_tcp:send(Socket, <<"Nickname already in use. Please choose another one: ">>),
                            nickname_loop(Socket)
                    end;
                error -> 
                    gen_tcp:send(Socket, <<"Nickname is not valid. Please retry: ">>),
                nickname_loop(Socket)
            end;

        {error, closed} ->
            error_logger:info_msg("Client disconnected~n"),
            gen_tcp:close(Socket);

        {error, Reason} -> 
            error_logger:info_msg("Uknown message format: ~p~n", [Reason]),
            nickname_loop(Socket)
    end.

% Starts hub loop
start_hub(Socket, Nickname) ->
    error_logger:info_msg("start_hub pid: ~p~n", [self()]),
    
    Parent = self(),
    spawn_link(fun() -> socket_reader(Socket, Parent) end),
    hub_loop(Socket, Nickname).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%%  HUB LOOP                                                                %%%
%%%  This is the section outside of a room, where the user can create rooms, %%%
%%%  close a room, invite a user and lot of more things                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hub_loop(Socket, Nickname) ->
    error_logger:info_msg("Entered hub_loop (~p)~n", [self()]),
    receive
        {user_input, Data} when is_binary(Data) ->
            Commands = binary:split(Data, <<" ">>, [global, trim]),
            case Commands of
                [<<"/create">>, Room] ->
                    room_registry:create_room(Room, Nickname, false),
                    gen_tcp:send(Socket, <<"Room created: ", Room/binary, "\r\n">>),
                    hub_loop(Socket, Nickname);

                [<<"/private">>, Room] ->
                    room_registry:create_room(Room, Nickname, true),
                    gen_tcp:send(Socket, <<"Private room created: ", Room/binary, "\r\n">>),
                    hub_loop(Socket, Nickname);
                
                [<<"/join">>, Room] ->
                    case room_registry:request_join_room(Room, Nickname) of
                        ok ->
                            gen_tcp:send(Socket, <<"You have joined room: ", Room/binary,"\r\n">>),
                            room_loop(Socket, Nickname, Room);
                        
                        {error, room_not_found} ->
                            gen_tcp:send(Socket, <<"Room not found\r\n">>),
                            hub_loop(Socket, Nickname);
                    
                        {error, not_invited} ->
                            gen_tcp:send(Socket, <<"You have not been invited to this room.\r\n">>),
                            hub_loop(Socket, Nickname);
                
                        {error, timeout} ->
                            gen_tcp:send(Socket, <<"Room did not respond.\r\n">>),
                            hub_loop(Socket, Nickname)
                    end;

                [<<"/close">>, Room] ->
                    room_registry:close_room(Room, Nickname),
                    gen_tcp:send(Socket, <<"Room closed: ", Room/binary, "\r\n">>),
                    hub_loop(Socket, Nickname);

                [<<"/list">>] ->
                    error_logger:info_msg("dentro list"),
                    Rooms = room_registry:list_rooms(),

                    AvailableRooms = lists:filter(
                        fun({_, false, _}) -> true;
                           ({_, true, Invited}) -> lists:member(Nickname, Invited)
                        end, Rooms),

                    gen_tcp:send(Socket, list_to_binary(io_lib:format("Available rooms: ~p\r\n", [AvailableRooms]))),
                    hub_loop(Socket, Nickname);

                [<<"/to">>, To | MessageParts] ->
                    Message = iolist_to_binary(join_with_space(MessageParts)),
                    case user_registry:get_pid(To) of
                        {ok, ToPid} ->
                            ToPid ! {private, Nickname, Message},
                            gen_tcp:send(Socket, <<"[To: ", To/binary, "] ", Message/binary, "\r\n">>);
                        
                        {error, user_not_found} ->
                            gen_tcp:send(Socket, <<"User not found\r\n">>)
                    end,
                    hub_loop(Socket, Nickname);
            
                [<<"/invite">>, Invited, RoomName] ->
                    case room_registry:get_room_pid(RoomName) of
                        {ok, RoomPid} -> 
                            case room_registry:get_owner(RoomName) of
                                {ok, Nickname} -> % if you are the owner
                                    case user_registry:get_pid(Invited) of
                                        {ok, ToPid} ->
                                            RoomPid ! {invite, Nickname, ToPid, Invited},
                                            gen_tcp:send(Socket, <<"Invitation sent to ", Invited/binary, "\r\n">>);
                           
                                        {error, user_not_found} ->
                                            gen_tcp:send(Socket, <<"User not found\r\n">>)
                                    end;

                                {ok, OtherUser} when OtherUser =/= Nickname ->
                                    gen_tcp:send(Socket, <<"You are not the owner of this room!\r\n">>);
                                error ->
                                    gen_tcp:send(Socket, <<"Error checking room ownership\r\n">>)
                            end;
                        
                        {error, not_found} ->
                            gen_tcp:send(Socket, <<"Room not found\r\n">>)
                    end,
                    hub_loop(Socket, Nickname);   
                
                [<<"/accept">>, RoomName] ->
                    case room_registry:get_room_pid(RoomName) of
                        {ok, RoomPid} ->
                            RoomPid ! {accept_invite, Nickname, self()},
                            receive
                                {join_ok} ->
                                    gen_tcp:send(Socket, <<"You have joined room: ", RoomName/binary, "\r\n">>),
                                    room_loop(Socket, Nickname, RoomName);
                                {join_denied} ->
                                    gen_tcp:send(Socket, <<"You are not invited to this room.\r\n">>),
                                    hub_loop(Socket, Nickname)
                            after 1000 ->
                                gen_tcp:send(Socket, <<"Room did not respond.\r\n">>),
                                hub_loop(Socket, Nickname)
                            end;
                        _ ->
                            gen_tcp:send(Socket, <<"Room not found\r\n">>),
                            hub_loop(Socket, Nickname)
                    end;

                [<<"/users">>] ->
                    Users = user_registry:list_users(),
                    Formatted = io_lib:format("Connected users: ~p\r\n", [Users]),
                    gen_tcp:send(Socket, list_to_binary(Formatted)),
                    hub_loop(Socket, Nickname);

                [<<"/help">>] ->
                    %% List of commands for hub
                    HelpTextHub = [
                      "/create [RoomName]     : create a public room\n",
                      "/private [RoomName]    : create a private room\n",
                      "/join [RoomName]       : join a room (if is private you need invitation)\n",
                      "/close [RoomName]      : closes room (only owner)\n",
                      "/list                  : show all available rooms for the user\n",
                      "/to [User] [Message]   : sends private message to User\n",
                      "/invite [User] [Room]  : invite User into your room (only owner)\n",
                      "/accept [RoomName]     : accept invite to RoomName and join it\n",
                      "/users                 chis chi: show list of connected users\n"
                      "/help                  : show help\n"
                    ],
                    gen_tcp:send(Socket, list_to_binary(HelpTextHub)),
                    hub_loop(Socket, Nickname);


                _ ->
                    gen_tcp:send(Socket, <<"Unknown command.\r\n">>),
                    hub_loop(Socket, Nickname)
            end;

        {private, From, Message} ->
            gen_tcp:send(Socket, <<"[From: ", From/binary, "] ", Message/binary, "\r\n">>),
            hub_loop(Socket, Nickname);

        {invitation, RoomName, From} ->
            gen_tcp:send(Socket, <<From/binary, " has invited you to the room: ", RoomName/binary,
                                    ". Type /accept ", RoomName/binary,
                                    " or /decline ", RoomName/binary, "\r\n">>),
            hub_loop(Socket, Nickname);

        {error, closed} ->
            error_logger:info_msg("Client disconnected~n"),
            user_registry:unregister(Nickname),
            gen_tcp:close(Socket);

        Other -> 
            error_logger:info_msg("Uknown message format: ~p~n", [Other]),
            hub_loop(Socket, Nickname)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                            %%%
%%%  ROOM LOOP                 %%%
%%%  This is the room chat     %%%
%%%                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
room_loop(Socket, Nickname, Room) ->
    receive
        {user_input, <<"/quit">>} ->
            room_registry:quit_room(Room, Nickname),
            gen_tcp:send(Socket, <<"Quitted room: ", Room/binary, "\r\n">>),
            start_hub(Socket, Nickname);

        {user_input, <<"/help">>} ->
            HelpTextRoom = [
              "/invite [User]     : invite User into current room (only for owner)\n",
              "/accept [RoomName] : accept invite to RoomName (will take you to that room)\n",
              "/quit              : leave room and go back to hub\n",
              "/users             : show list of connected users\n"        
              "/help              : show help\n"
            ],
            gen_tcp:send(Socket, list_to_binary(HelpTextRoom)),
            room_loop(Socket, Nickname, Room);

        {user_input, <<"/users">>} ->
            Users = user_registry:list_users(),
            Formatted = io_lib:format("Connected users: ~p\r\n", [Users]),
            gen_tcp:send(Socket, list_to_binary(Formatted)),
            hub_loop(Socket, Nickname);

        {user_input, Data} when is_binary(Data) ->
            Commands = binary:split(Data, <<" ">>, [global, trim]),
            case Commands of
                [<<"/invite">>, Invited] ->
                    case room_registry:get_owner(Room) of
                        {ok, Owner} when Owner =:= Nickname ->
                            case user_registry:get_pid(Invited) of
                                {ok, ToPid} ->
                                    case room_registry:get_room_pid(Room) of
                                        {ok, RoomPid} ->
                                            RoomPid ! {invite, Nickname, ToPid, Invited};
                                        {error, not_found} ->
                                            gen_tcp:send(Socket, <<"Room not found\r\n">>)
                                    end,

                                    gen_tcp:send(Socket, <<"Invitation sent to ", Invited/binary, "\r\n">>);
                                {error, user_not_found} ->
                                    gen_tcp:send(Socket, <<"User not found\r\n">>)
                            end;

                        {ok, _Other} ->
                            gen_tcp:send(Socket, <<"You are not the owner of this room!\r\n">>);

                        {error, _} ->
                            gen_tcp:send(Socket, <<"Error checking room ownership\r\n">>)
                    end,
                    room_loop(Socket, Nickname, Room); 

                [<<"/accept">>, OtherRoom] ->
                    if OtherRoom =:= Room ->
                        % The user is already into the room he has been invited to join
                        gen_tcp:send(Socket, <<"You are already in room: ", Room/binary, "\r\n">>),
                        room_loop(Socket, Nickname, Room);
                    
                    true ->
                        % Send accept_invite to the OtherRoom
                        case room_registry:get_room_pid(OtherRoom) of
                            {ok, OtherPid} ->
                                OtherPid ! {accept_invite, Nickname, self()},
                                receive 
                                    {join_ok} ->
                                        room_registry:quit_room(Room, Nickname),
                                        gen_tcp:send(Socket, <<"Switched to room: ", OtherRoom/binary, "\r\n">>),
                                        room_loop(Socket, Nickname, OtherRoom);
                                    
                                    {join_denied} ->
                                        gen_tcp:send(Socket, <<"You are not invited to room: ", OtherRoom/binary, "r\n">>),
                                        room_loop(Socket, Nickname, Room)
                                    
                                after 1000 ->
                                    gen_tcp:send(Socket, <<"Room did not respond.\r\n">>),
                                    room_loop(Socket, Nickname, Room)
                                end;

                            {error, not_found} ->
                                gen_tcp:send(Socket, <<"Room not found\r\n">>),
                                room_loop(Socket, Nickname, Room)
                        end
                    end,
                    room_loop(Socket, Nickname, Room);

                [Message] ->
                    room_registry:broadcast(Room, Nickname, Message),
                    room_loop(Socket, Nickname, Room);

                _ -> 
                    gen_tcp:send(Socket, <<"Unknown command inside room.\r\n">>),
                    room_loop(Socket, Nickname, Room)
            end;

        {message, Room, From, Msg} ->
            gen_tcp:send(Socket, <<"[", Room/binary, "] ", From/binary, ": ", Msg/binary, "\r\n">>),
            room_loop(Socket, Nickname, Room);

        {socket_closed} ->
            error_logger:info_msg("Client ~p disconnected~n", [Nickname]),
            room_registry:quit_room(Room, Nickname),
            gen_tcp:close(Socket);

        {info, Message} ->
            gen_tcp:send(Socket, Message),
            room_loop(Socket, Nickname, Room);

        {invitation, OtherRoom, From} ->
            gen_tcp:send(Socket, <<From/binary, " has invited you to room: ", OtherRoom/binary,
                                    ". Type /accept ", OtherRoom/binary, " to join.\r\n">>),
            room_loop(Socket, Nickname, Room);

        Other ->
            error_logger:info_msg("Unexpected message: ~p~n", [Other]),
            room_loop(Socket, Nickname, Room)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%  HELPER METHODS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% SOCKET READER
%%%% Since gen_tcp:recv/2 is blocking, we cannot call it in the main process, otherwise it would block waiting for input. 
%%%% This separates the reading in the socket_Reader process (valid both for hub_loop and room_loop)
socket_reader(Socket, Parent) ->
    error_logger:info_msg("socket_reader parent: ~p~n", [Parent]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            error_logger:info_msg("Received message"),
            Message = binary:replace(Data, <<"\r\n">>, <<>>, [global]),
            error_logger:info_msg("Sending: ~p~n", [Message]),
            Parent ! {user_input, Message},
            socket_reader(Socket, Parent);
        
        {error, closed} ->
            Parent ! {socket_closed};
        
        {error, ealready} -> 
            Parent ! {socket_closed}
    end.

% Parses nickname with trimming
parse_nickname(<<>>) ->
    error;
parse_nickname(Data) when is_binary(Data) ->
    Trimmed = binary:replace(Data, <<"\r\n">>, <<>>, [global]),
    case Trimmed of
        <<>> -> error;
        _ -> {ok, Trimmed}
    end.

% Join words separated by space
join_with_space([]) -> [];
join_with_space([H]) -> [H];
join_with_space([H | T]) -> [H, <<" ">> | join_with_space(T)].
