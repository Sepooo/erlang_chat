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

        {tcp, Socket, _Data} when Nickname =/= undefined ->
            recv_loop(Socket, Nickname);

        {error, closed} ->
            io:format("Client disconnected~n"),
            gen_tcp:close(Socket);

        {error, Reason} -> 
            io:format("Uknown message format: ~p~n", [Reason]),
            recv_loop(Socket, undefined)
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