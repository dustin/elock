-module (lock_tcp_listener_app).

-behaviour (application).

% Supervisor functions.
-export([start/0,
    start/2, stop/1, config_change/3, start_phase/3, prep_stop/1]).

% easy start
start() ->
    application:start(lock_tcp_listener).

% application stuff
start(_Type, _Args) ->
    error_logger:info_msg("Starting lock_tcp_listener", []),
    lock_tcp_listener:start_link().

stop(LockPid) ->
    error_logger:info_msg("Stopping lock_tcp_listener~n", []),
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
    error_logger:info_msg("Prepping stop of lock_tcp_listener~n", []),
    State.
