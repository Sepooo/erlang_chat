-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

% Test 1: checks if supervisor starts correctly
start_stop_supervisor_test() ->
    {ok, Pid} = erlang_otp_chat_sup:start_link(),
    ?assert(is_pid(Pid)),
    exit(Pid, normal),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

% Test 2: start TCP worker
tcp_worker_start_test() ->
    {ok, PidSup} = erlang_otp_chat_sup:start_link(),
    {ok, PidWorker} = erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),
    ?assert(is_pid(PidWorker)),
    exit(PidSup, normal),
    
    timer:sleep(100),
    
    ?assertNot(is_process_alive(PidSup)),
    ?assertNot(is_process_alive(PidWorker)).

% Test 3: connection to TCP and input Nickname
tcp_echo_test() ->
    user_registry:start_link(),

    {ok, PidSup} = erlang_otp_chat_sup:start_link(),
    {ok, PidWorker} = erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),

    timer:sleep(100),

    {ok, Socket} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    {ok, InitialData} = gen_tcp:recv(Socket, 0),

    ?assertMatch(<<"Please, enter your nickname: ">>, InitialData),

    gen_tcp:send(Socket, <<"Alice\r\n">>),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    ?assertEqual(<<"Welcome Alice!\nType /help for commands\r\n">>, Reply),

    gen_tcp:close(Socket),
    exit(PidSup, normal),
    
    timer:sleep(100),
    
    ?assertNot(is_process_alive(PidSup)),
    ?assertNot(is_process_alive(PidWorker)).

% Test 4: room creation
room_creation_test() ->
    {ok, Pid} = room:start_link(<<"test_room">>, alice, false),
    ?assert(is_pid(Pid)),
    exit(Pid, normal).

% Test 5: user joins a room
join_user_test() ->
    flush_mailbox(),
    {ok, RoomPid} = room:start_link(<<"room1">>, alice, false),
    TestPid = self(),
    
    % Spawn a fake client to simulate the user
    ClientPid = spawn(fun() -> 
        receive 
            Message -> 
                TestPid ! Message
        end 
    end),

    RoomPid ! {request_join, bob, ClientPid},

    receive
        {join_ok} ->
            ?assert(true)
    after 1000 ->
        ?assert(false)
    end,
    exit(RoomPid, normal).

% Test 6: broadcast a message into a room
broadcast_message_test() ->
    {ok, RoomPid} = room:start_link(<<"room2">>, alice, false),
    TestPid = self(),

    % fake client that sends messages
    ClientPid = spawn(fun() -> client_loop(TestPid) end),

    RoomPid ! {request_join, bob, ClientPid},

    receive {join_ok} -> ok
    after 1000 -> ?assert(false) end,
    
    RoomPid ! {broadcast, bob, <<"Hello everyone!">>},
    
    receive
        {message, <<"room2">>, bob, <<"Hello everyone!">>} -> ok
    after 1000 -> ?assert(false) end,

    exit(RoomPid, normal).

client_loop(TestPid) ->
    receive
        Message ->
            TestPid ! Message,
            client_loop(TestPid)
    end.

% Test 7: join and quit room
quit_room_test() ->
    flush_mailbox(),
    {ok, RoomPid} = room:start_link(<<"room3">>, alice, false),
    Self = self(),
    RoomPid ! {request_join, charlie, Self},
    
    receive 
        {join_ok} -> 
            ok 
        after 1000 -> 
            ?assert(false) 
    end,
    
    RoomPid ! {quit, charlie},
    exit(RoomPid, normal).

% Test 8: owner closes room
close_room_by_owner_test() ->
    flush_mailbox(),
    {ok, RoomPid} = room:start_link(<<"room4">>, diana, false),
    Self = self(),
    RoomPid ! {request_join, eric, Self},

    receive 
        {join_ok} -> 
            ok 
        after 1000 -> 
            ?assert(false) 
    end,

    RoomPid ! {close, diana},
    
    receive
        {info, <<"Room closed by owner.">>} ->
            ok
    after 1000 ->
        ?assert(false)
    end.

% Test 9: client connects and sends command /create
create_room_command_test() ->
    room_registry:start_link(),

    {ok, PidSup} = erlang_otp_chat_sup:start_link(),
    {ok, _PidWorker} = erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),
    timer:sleep(100),

    {ok, Socket} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),

    gen_tcp:recv(Socket, 0), % recive prompt
    gen_tcp:send(Socket, <<"Paolo\r\n">>),
    gen_tcp:recv(Socket, 0), % welcome message

    gen_tcp:send(Socket, <<"/create myroom\r\n">>), %c client sends command
    {ok, Response} = gen_tcp:recv(Socket, 0),

    ?assertEqual(<<"Room created: myroom\r\n">>, Response),

    gen_tcp:close(Socket),
    exit(PidSup, normal),
    timer:sleep(100).


% Test 10: two clients create two rooms, and another client send /list
list_rooms_command_test() ->
    {ok, Sup} = erlang_otp_chat_sup:start_link(),
    {ok, _} = erlang_otp_chat_sup:start_tcp_worker(undefined, {127,0,0,1}, 8080, []),
    timer:sleep(100),

    create_client_with_cmd("user1", <<"/create roomA\r\n">>),
    create_client_with_cmd("user2", <<"/create roomB\r\n">>),

    % third client
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    gen_tcp:recv(Sock, 0), % receive prompt
    gen_tcp:send(Sock, <<"user3\r\n">>),
    gen_tcp:recv(Sock, 0), % welcome message

    gen_tcp:send(Sock, <<"/list\r\n">>),
    {ok, Response} = gen_tcp:recv(Sock, 0),

    ?assert(binary:match(Response, <<"roomA">>) /= nomatch),
    ?assert(binary:match(Response, <<"roomB">>) /= nomatch),

    gen_tcp:close(Sock),
    exit(Sup, normal),
    timer:sleep(100).

% Test 11: simulates a private message exchange between two users
private_message_test() ->
    {ok, Sup} = erlang_otp_chat_sup:start_link(),
    {ok, _} = erlang_otp_chat_sup:start_tcp_worker(undefined, {127,0,0,1}, 8080, []),
    timer:sleep(100),

    % Client 1
    {ok, Sock1} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    gen_tcp:recv(Sock1, 0),
    gen_tcp:send(Sock1, <<"Pippo\r\n">>),
    gen_tcp:recv(Sock1, 0),

    % Client 2
    {ok, Sock2} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    gen_tcp:recv(Sock2, 0),
    gen_tcp:send(Sock2, <<"Pluto\r\n">>),
    gen_tcp:recv(Sock2, 0),

    % Pippo sends message to Pluto
    gen_tcp:send(Sock2, <<"/to Pippo Ciao pippo!\r\n">>),
    {ok, RespPippo} = gen_tcp:recv(Sock1, 0),
    ?assertMatch(<<"[From: Pluto] Ciao pippo!\r\n">>, RespPippo),

    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    exit(Sup, normal),
    timer:sleep(100).

%%%%%%%% =========== PRIVATE ROOMS =========== %%%%%%%%
%% Test 12: only owner can send invitations in private room
only_owner_can_invite_test() ->
    {ok, Sup} = erlang_otp_chat_sup:start_link(),
    {ok, _}  = erlang_otp_chat_sup:start_tcp_worker(undefined, {127,0,0,1}, 8080, []),
    timer:sleep(100),

    %% Alice creates private room
    {ok, SockAlice} = gen_tcp:connect("localhost", 8080, [binary, {active,false}]),
    gen_tcp:recv(SockAlice, 0),
    gen_tcp:send(SockAlice, <<"alice\r\n">>),
    gen_tcp:recv(SockAlice, 0),
    gen_tcp:send(SockAlice, <<"/private secretroom\r\n">>),
    gen_tcp:recv(SockAlice, 0), 

    %% Bob connects
    {ok, SockBob} = gen_tcp:connect("localhost", 8080, [binary, {active,false}]),
    gen_tcp:recv(SockBob, 0),
    gen_tcp:send(SockBob, <<"bob\r\n">>),
    gen_tcp:recv(SockBob, 0),

    %% Alice invites Bob into her private room
    gen_tcp:send(SockAlice, <<"/invite bob secretroom\r\n">>),
    gen_tcp:recv(SockAlice, 0), 
    {ok, InviteMsg} = gen_tcp:recv(SockBob, 0),
    ?assertMatch(<<"alice has invited you to the room: secretroom. Type /accept secretroom or /decline secretroom\r\n">>, InviteMsg),

    %% Bob joins Alice's private room
    gen_tcp:send(SockBob, <<"/join secretroom\r\n">>),
    gen_tcp:recv(SockBob, 0),

    %% Charlie connects
    {ok, SockCharlie} = gen_tcp:connect("localhost", 8080, [binary, {active,false}]),
    gen_tcp:recv(SockCharlie, 0),
    gen_tcp:send(SockCharlie, <<"charlie\r\n">>),
    gen_tcp:recv(SockCharlie, 0),

    %% Bob tries to invite Charlie (but he is not the owner)
    gen_tcp:send(SockBob, <<"/invite charlie\r\n">>),
    {ok, Resp} = gen_tcp:recv(SockBob, 0),
    ?assertEqual(<<"You are not the owner of this room!\r\n">>, Resp),

    %% cleanup
    gen_tcp:close(SockAlice),
    gen_tcp:close(SockBob),
    gen_tcp:close(SockCharlie),
    exit(Sup, normal),
    timer:sleep(100).

%% Test 13: user cannot join private room without invitation
cannot_join_private_room_without_invite_test() ->
    {ok, Sup} = erlang_otp_chat_sup:start_link(),
    {ok, _}  = erlang_otp_chat_sup:start_tcp_worker(undefined, {127,0,0,1}, 8080, []),
    timer:sleep(100),

    %% Alice creates private room
    {ok, SockAlice} = gen_tcp:connect("localhost", 8080, [binary, {active,false}]),
    gen_tcp:recv(SockAlice, 0),
    gen_tcp:send(SockAlice, <<"alice2\r\n">>),
    gen_tcp:recv(SockAlice, 0),
    gen_tcp:send(SockAlice, <<"/private alice2room\r\n">>),
    gen_tcp:recv(SockAlice, 0),

    %% Bob connects but he is not invited
    {ok, SockBob} = gen_tcp:connect("localhost", 8080, [binary, {active,false}]),
    gen_tcp:recv(SockBob, 0),
    gen_tcp:send(SockBob, <<"bob2\r\n">>),
    {ok, Response1} = gen_tcp:recv(SockBob, 0),

    %% Bob tries to join secret room
    gen_tcp:send(SockBob, <<"/join alice2room\r\n">>),
    {ok, Response} = gen_tcp:recv(SockBob, 0),
    error_logger:info_msg(Response1, Response),
    ?assertEqual(<<"You have not been invited to this room.\r\n">>, Response),

    %% cleanup
    gen_tcp:close(SockAlice),
    gen_tcp:close(SockBob),
    exit(Sup, normal),
    timer:sleep(100).



%%%%%%%% =========== HELPER FUNCTIONS =========== %%%%%%%%
%%% 
% helper function to create a new client who registers and sends a command
create_client_with_cmd(Nick, Command) ->
    {ok, Sock} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    gen_tcp:recv(Sock, 0),

    NickBin = list_to_binary(Nick), 
    gen_tcp:send(Sock, <<NickBin/binary, "\r\n">>),
    
    gen_tcp:recv(Sock, 0),
    gen_tcp:send(Sock, Command),
    gen_tcp:recv(Sock, 0),
    gen_tcp:close(Sock).

% helper function to clear process mailbox
flush_mailbox() ->
    receive
        _Any -> flush_mailbox()
    after 0 -> ok
    end.
