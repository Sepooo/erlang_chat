%%%-------------------------------------------------------------------
%% @doc erlang_otp_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_otp_chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_otp_chat_sup:start_link(), % Starts main supervisor
    erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),
    room_registry:start_link(), % Initialize ETS for rooms
    user_registry:start_link(). % Initialize ETS for users

stop(_State) ->
    ok.