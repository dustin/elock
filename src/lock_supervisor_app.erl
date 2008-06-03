-module(lock_supervisor_app).

-behaviour (application).

% Supervisor functions.
-export([start/0,
    start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
    application:start(lock_supervisor).

% application stuff
start(_Type, _Args) ->
    error_logger:info_msg("Starting lock_supervisor~n", []),
    supervisor:start_link(gen_sup, [
            {{rest_for_one, 2, 60},
                [{lock_serv, {lock_serv, start_link, []},
                    permanent, 5000, worker, [lock_serv]},
                 {lock_tcp_listener, {lock_tcp_listener, start_link, []},
                    permanent, 5000, worker, [lock_tcp_listener]}
                ]}]).

stop(LockPid) ->
    error_logger:info_msg("Stopping lock_supervisor~n", []),
    LockPid ! stop,
    ok.

config_change(Changed, New, Removed) ->
    error_logger:info_msg("Config changed:  [~p, ~p, ~p]",
        [Changed, New, Removed]),
    ok.

start_phase(Phase, StartType, PhaseArgs) ->
    error_logger:info_msg("start_phase:  [~p, ~p, ~p]",
        [Phase, StartType, PhaseArgs]),
    ok.

prep_stop(State) ->
    error_logger:info_msg("Prepping stop of lock_supervisor~n", []),
    State.
