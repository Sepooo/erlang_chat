-module(chat_server_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

% Test 1: verifica che il supervisor principale parta correttamente
start_stop_supervisor_test() ->
    {ok, Pid} = erlang_otp_chat_sup:start_link(),
    ?assert(is_pid(Pid)),
    exit(Pid, normal),
    timer:sleep(100),
    ?assertNot(is_process_alive(Pid)).

% Test 2: avvio del worker TCP
tcp_worker_start_test() ->
    {ok, PidSup} = erlang_otp_chat_sup:start_link(),
    {ok, PidWorker} = erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),
    ?assert(is_pid(PidWorker)),
    exit(PidSup, normal),
    
    timer:sleep(100),
    
    ?assertNot(is_process_alive(PidSup)),
    ?assertNot(is_process_alive(PidWorker)).

% Test 3: connessione TCP e input Nickname
tcp_echo_test() ->
    {ok, PidSup} = erlang_otp_chat_sup:start_link(),
    {ok, PidWorker} = erlang_otp_chat_sup:start_tcp_worker(undefined, {0,0,0,0}, 8080, []),

    timer:sleep(100),

    {ok, Socket} = gen_tcp:connect("localhost", 8080, [binary, {active, false}]),
    {ok, InitialData} = gen_tcp:recv(Socket, 0),

    ?assertMatch(<<"Please, enter your nickname: ">>, InitialData),

    gen_tcp:send(Socket, <<"Alice\r\n">>),
    {ok, Reply} = gen_tcp:recv(Socket, 0),
    ?assertEqual(<<"Welcome Alice!\r\n">>, Reply),

    gen_tcp:close(Socket),
    exit(PidSup, normal),
    
    timer:sleep(100),
    
    ?assertNot(is_process_alive(PidSup)),
    ?assertNot(is_process_alive(PidWorker)).