%%%-------------------------------------------------------------------
%% @doc erlang_otp_chat top level supervisor.
%%      Nicolò Priano 1/06/25
%% @end
%%%-------------------------------------------------------------------

-module(erlang_otp_chat_sup).

-behaviour(supervisor).

-export([start_link/4, start_child/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Callback, IP, Port, UserArgs) ->
    {ok, Pid} = supervisor:start_link(?MODULE, [Port]),
    start_child(Pid),
    {ok, Pid}.

start_child(Server) ->
    supervisor:start_child(Server, []).

init([Callback, IP, Port, UserArgs]) ->
%    SocketOptions = [binary, {active, false}, {reuseaddr, true}, {ip, IP}],
%    {ok, LSock} = gen_tcp:listen(Port, SocketOptions),
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpec = #{
        id => tcp_worker,
        start => {tcp_worker, start_link, [Callback, LSock, UserArgs]},
        restart => transient,
        type => worker,
        modules => [tcp_worker]
    },
    {ok, {SupFlags, [ChildSpec]}}.
