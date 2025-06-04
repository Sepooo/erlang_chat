-module(chat_client_handler).

-export([start/1]).

start(Socket) ->
    inet:setopts(Socket, [{active, false}, binary]),
    gen_tcp:send(Socket, <<"Please, enter your nickname: ">>),
    recv_loop(Socket, undefined).

recv_loop(Socket, Nickname) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} when Nickname =:= undefined ->
            io:format("Received data: ~p~n", [Data]),
            case parse_nickname(Data) of
                {ok, NewNick} ->
                    gen_tcp:send(Socket, <<"Welcome ", NewNick/binary, "!\r\n">>),
                    recv_loop(Socket, NewNick);
                error -> 
                    gen_tcp:send(Socket, <<"Nickname is not valid. Please retry: ">>),
                recv_loop(Socket, undefined)
            end;

        {ok, Data} when is_binary(Data) ->
            Command = binary:replace(Data, <<"\r\n">>, <<>>, [global]),
            case binary:split(Command, <<":">>, [global]) of
                [<<"CREATE">>, Room] ->
                    room_registry:create_room(Room, Nickname),
                    gen_tcp:send(Socket, <<"Room created: ", Room/binary, "\r\n">>);
                
                [<<"JOIN">>, Room] ->
                    case room_registry:join_room(Room, Nickname) of
                        ok ->
                            recv_loop(Socket, Nickname, Room);
                        {error, room_not_found} ->
                            gen_tcp:send(Socket, <<"Room not found\r\n">>)
                    end;

                [<<"QUIT">>, Room] ->
                    room_registry:quit_room(Room, Nickname),
                    gen_tcp:send(Socket, <<"Quitted room: ", Room/binary, "\r\n">>);

                [<<"CLOSE">>, Room] ->
                    room_registry:close_room(Room, Nickname),
                    gen_tcp:send(Socket, <<"Room closed: ", Room/binary, "\r\n">>);

                [<<"LIST">>] ->
                    Rooms = room_registry:list_rooms(),
                    gen_tcp:send(Socket, list_to_binary(io_lib:format("Rooms: ~p\r\n", [Rooms])));
                _ ->
                    gen_tcp:send(Socket, <<"Unknown command.\r\n">>)
            end,
            recv_loop(Socket, Nickname);

        {tcp, Socket, _Data} when Nickname =/= undefined ->
            recv_loop(Socket, Nickname);

        {error, closed} ->
            io:format("Client disconnected~n"),
            gen_tcp:close(Socket);

        {error, Reason} -> 
            io:format("Uknown message format: ~p~n", [Reason]),
            recv_loop(Socket, undefined)
    end.

recv_loop(Socket, Nickname, Room) ->
    Parent = self(),
    %% Spawna processo reader
    spawn_link(fun() -> socket_reader(Socket, Parent) end),
    process_loop(Socket, Nickname, Room).

socket_reader(Socket, Parent) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Message = binary:replace(Data, <<"\r\n">>, <<>>, [global]),
            Parent ! {user_input, Message},
            socket_reader(Socket, Parent);
        {error, closed} ->
            Parent ! {socket_closed}
    end.


process_loop(Socket, Nickname, Room) ->
    receive
        {user_input, <<"/quit">>} ->
            room_registry:quit_room(Room, Nickname),
            gen_tcp:send(Socket, <<"Quitted room: ", Room/binary, "\r\n">>),
            gen_tcp:close(Socket);

        {user_input, Msg} ->
            room_registry:broadcast(Room, Nickname, Msg),
            process_loop(Socket, Nickname, Room);

        {message, Room, From, Msg} ->
            gen_tcp:send(Socket, <<"[", Room/binary, "] ", From/binary, ": ", Msg/binary, "\r\n">>),
            process_loop(Socket, Nickname, Room);

        {socket_closed} ->
            io:format("Client ~p disconnected~n", [Nickname]),
            room_registry:quit_room(Room, Nickname),
            gen_tcp:close(Socket);

        {info, Message} ->
            gen_tcp:send(Socket, Message),
            process_loop(Socket, Nickname, Room);

        Other ->
            io:format("Unexpected message: ~p~n", [Other]),
            process_loop(Socket, Nickname, Room)
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