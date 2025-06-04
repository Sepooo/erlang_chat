# Erlang/OTP Chat application
[![Erlang/OTP Version](https://img.shields.io/badge/erlang-26%2B-blue)](https://www.erlang.org)
[![Rebar3](https://img.shields.io/badge/rebar3-3.25%2B-orange)](https://rebar3.org)

## Prerequisites
- Erlang/OTP 28
- rebar3

## How to install
        git clone https://github.com/TUO_USER/erlang_otp_chat.git
        cd erlang_otp_chat

## How to compile and release
        rebar3 compile
        rebar3 as prod release

## How to run the application
* From rebar3
            
            rebar3 shell
            application:start(erlang_otp_chat).

* Run as Daemon (background process)
            
            _build/prod/rel/erlang_otp_chat/bin/erlang_otp_chat start
    
    to stop it:
            
            _build/prod/rel/erlang_otp_chat/bin/erlang_otp_chat stop

## Unit test
        rebar3 eunit

## Integration test 
        rebar3 ct
