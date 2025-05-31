%%%-------------------------------------------------------------------
%% @doc erlang_otp_chat public API
%% @end
%%%-------------------------------------------------------------------

-module(erlang_otp_chat_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    erlang_otp_chat_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
