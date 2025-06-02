-module(tcp_worker).
-behaviour(gen_server).

% Funzione pubblica
-export([start_link/4]).

% Callback gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Callback, IP, Port, UserArgs) ->
    gen_server:start_link(?MODULE, [Callback, IP, Port, UserArgs], []).

init([Callback, IP, Port, UserArgs]) ->
    SocketOptions = [binary, {active, false}, {reuseaddr, true}, {ip, IP}],
    case gen_tcp:listen(Port, SocketOptions) of 
        {ok, LSock} ->
            self() ! accept,
            {ok, #{sock => LSock, callback => Callback, args => UserArgs}};

        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #{sock := LSock} = State) ->
    case gen_tcp:accept(LSock) of
        {ok, ClientSocket} ->
            %% Qui dovresti spawnare un handler per il client
            io:format("Nuova connessione: ~p~n", [ClientSocket]),
            %% Continua ad accettare connessioni
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            {stop, normal, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #{sock := LSock}) ->
    gen_tcp:close(LSock),
    ok.