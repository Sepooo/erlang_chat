# Erlang/OTP Chat application
[![Erlang/OTP Version](https://img.shields.io/badge/erlang-25-blue)](https://www.erlang.org)
[![Rebar3](https://img.shields.io/badge/rebar3-3.25%2B-orange)](https://rebar3.org)
[![Python3](https://img.shields.io/badge/python3-blue)](https://rebar3.org)

## Prerequisites
- Erlang/OTP 25
- rebar3

## How to install
        git clone https://github.com/Sepooo/erlang_chat.git
        cd erlang_otp_chat

## How to compile and release
        rebar3 compile
        rebar3 as prod release

## How to run the application
* From rebar3
            
            rebar3 shell

## How to connect via client
* Via python3 client

Inside the uploaded zip folder you can find the script client.py, run it with:

                  python3 client.py

* Or, if you have telnet installed
                
                telnet localhost 8080
## Unit test
        rebar3 eunit
        
## How to use erlang_chat

1 - After running the server, join by using the client application client.py. The server will ask you to insert your nickname.

2 - Then, you will be taken into the hub, where you type those commands:

        /create [RoomName]     : create a public room,
        /private [RoomName]    : create a private room,
        /join [RoomName]       : join a room (if is private you need invitation),
        /close [RoomName]      : closes room (only owner),
        /list                  : show all available rooms for the user,
        /to [User] [Message]   : sends private message to User,
        /invite [User] [Room]  : invite User into your room (only owner),
        /accept [RoomName]     : accept invite to RoomName and join it,
        /users                 chis chi: show list of connected users
        /help                  : show help

3 - When you are inside a room, you can:

        /invite [User]     : invite User into current room (only for owner),
        /accept [RoomName] : accept invite to RoomName (will take you to that room),
        /quit              : leave room and go back to hub,
        /users             : show list of connected users        
        /help              : show help