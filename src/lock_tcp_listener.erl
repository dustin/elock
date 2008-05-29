-module (lock_tcp_listener).

-export([start/0, start/1, start_link/0, start_link/1, init/1]).

% Starting the server
start() ->
    start(11400).

start(PortNum) when integer(PortNum) ->
    {ok, spawn(?MODULE, init, [PortNum])}.

start_link() ->
    start_link(11400).

start_link(PortNum) when integer(PortNum) ->
    {ok, spawn_link(?MODULE, init, [PortNum])}.


%
% The server itself
%

% server self-init
init(PortNum) ->
    {ok, LS} = gen_tcp:listen(PortNum, [{reuseaddr, true}, {packet, 0},
                                    {active, false}]),
    accept_loop(LS).

% Accept incoming connections
accept_loop(LS) ->
    {ok, NS} = gen_tcp:accept(LS),
    Pid = spawn(lock_connection, lock, [NS]),
    gen_tcp:controlling_process(NS, Pid),
    Pid ! go_ahead,
    % Check to see if there's a stop message.
    receive
        stop ->
            error_logger:info_msg("lock_serv received stop message.~n", []),
            gen_tcp:shutdown(LS, read_write);
        M ->
            error_logger:error_msg("Received unexpected message: ~p~n", [M]),
            accept_loop(LS)
        after 1 ->
            accept_loop(LS)
    end.

