%%%-------------------------------------------------------------------
%% @doc erlang_otp_chat top level supervisor.
%%      Nicolò Priano 1/06/25
%% @end
%%%-------------------------------------------------------------------

-module(erlang_otp_chat_sup).

-behaviour(supervisor).

-export([start_link/0, start_tcp_worker/4]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% Function that starts a new server TCP
start_tcp_worker(Callback, IP, Port, UserArgs) ->
    supervisor:start_child(?MODULE, [Callback, IP, Port, UserArgs]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 0,
        period => 1
    },
    ChildSpec = #{
        id => tcp_worker,
        start => {tcp_worker, start_link, []},
        restart => transient, % do not reboot if terminates manually
        type => worker,
        modules => [tcp_worker]
    },
    {ok, {SupFlags, [ChildSpec]}}.
