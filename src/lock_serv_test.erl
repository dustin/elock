-module (lock_serv_test).

-export ([start/0, child_loop/0]).

-include ("lock_stats.hrl").

child_loop() ->
    receive
        {Sender, lock, [Thing, Timeout]} ->
            Sender ! {res, lock_serv:lock(Thing, Timeout)},
            child_loop();
        {Sender, lock, [Thing]} ->
            Sender ! {res, lock_serv:lock(Thing)},
            child_loop();
        {Sender, unlock, [Thing]} ->
            Sender ! {res, lock_serv:unlock(Thing)},
            child_loop();
        {Sender, unlock_all, []} ->
            Sender ! {res, lock_serv:unlock_all()},
            child_loop();
        {Sender, get_locker_id, []} ->
            Sender ! {res, lock_serv:get_locker_id()},
            child_loop();
        {Sender, set_timeout, [Millis]} ->
            Sender ! {res, lock_serv:set_timeout(Millis)},
            child_loop();
        stop -> ok
    end.

rpc(Child, Message, Args, Timeout) ->
    Child ! {self(), Message, Args},
    receive
        {res, M} -> M
        after Timeout ->
            exit(lists:flatten(
                io_lib:format("Timed out waiting for ~p", [Message])))
    end.

rpc(Child, Message, Args) -> rpc(Child, Message, Args, 100).

test_basic_lock(Child1, Child2) ->
    ok = rpc(Child1, lock, ["test"]),
    locked = rpc(Child2, lock, ["test"]),
    not_yours = rpc(Child2, unlock, ["test"]),
    ok = rpc(Child1, unlock, ["test"]),
    not_locked = rpc(Child1, unlock, ["test"]).

test_double_lock(Child1, Child2) ->
    ok = rpc(Child1, lock, ["test"]),
    ok = rpc(Child1, lock, ["test"]),
    locked = rpc(Child2, lock, ["test"]),
    not_yours = rpc(Child2, unlock, ["test"]),
    ok = rpc(Child1, unlock, ["test"]),
    not_locked = rpc(Child1, unlock, ["test"]).

test_delayed_lock(Child1, Child2) ->
    ok = rpc(Child1, lock, ["test", 1000]),
    locked = rpc(Child2, lock, ["test", 10]),
    not_yours = rpc(Child2, unlock, ["test"]),
    ok = rpc(Child1, unlock, ["test"]).

test_delayed_lock_release(Child1, Child2) ->
    Key = "dr_test",
    ok = rpc(Child1, lock, [Key]),
    {ok, _Tref} = timer:send_after(5, Child1, {self(), unlock, [Key]}),
    ok = rpc(Child2, lock, [Key, 50]),
    % Drain the message from the deferred delete
    receive {res, ok} -> ok end,
    not_yours = rpc(Child1, unlock, [Key]),
    ok = rpc(Child2, unlock, [Key]).

test_delayed_lock_release_with_third_child(Child1, Child2) ->
    Key = "dr3_test",
    Child3 = spawn_link(?MODULE, child_loop, []),
    ok = rpc(Child1, lock, [Key]),
    {ok, _Tref} = timer:send_after(5, Child1, {self(), unlock, [Key]}),
    Child2 ! {self(), lock, [Key, 50]},
    Child3 ! {self(), lock, [Key, 50]},
    receive {res, ok} -> ok end, % child1 lock
    receive {res, ok} -> ok end, % rpc call
    receive {res, locked} -> ok end, % child3 call
    not_yours = rpc(Child1, unlock, [Key]),
    not_yours = rpc(Child3, unlock, [Key]),
    ok = rpc(Child2, unlock, [Key]),
    Child3 ! stop.

test_unlock_all(Child1, Child2) ->
    ok = rpc(Child1, lock, ["key1"]),
    ok = rpc(Child1, lock, ["key2"]),
    locked = rpc(Child2, lock, ["key1"]),
    locked = rpc(Child2, lock, ["key2"]),
    ok = rpc(Child1, unlock_all, []),
    ok = rpc(Child2, lock, ["key1"]),
    ok = rpc(Child2, lock, ["key2"]).

test_unlock_all_empty(Child1, _Child2) ->
    ok = rpc(Child1, unlock_all, []).

test_delayed_unlock_all(Child1, Child2) ->
    ok = rpc(Child1, lock, ["key1"]),
    ok = rpc(Child1, lock, ["key2"]),
    {ok, _Tref} = timer:send_after(5, Child1, {self(), unlock_all, []}),
    ok = rpc(Child2, lock, ["key1", 50]),
    receive {res, ok} -> ok end,
    ok = rpc(Child2, lock, ["key2"]).

test_dead_clients_hold_no_locks(Child1, Child2) ->
    ok = rpc(Child1, lock, ["key1"]),
    ok = rpc(Child1, lock, ["key2"]),
    {ok, _Tref} = timer:send_after(5, Child1, stop),
    ok = rpc(Child2, lock, ["key1", 50]),
    ok = rpc(Child2, lock, ["key2"]).

test_dead_clients_lose_registration(Child1, _Child2) ->
    rpc(Child1, get_locker_id, []),
    S = lock_serv:stats(),
    N = S#stats.clients,
    N = S#stats.monitoring,
    Child1 ! stop,
    timer:sleep(250),
    S2 = lock_serv:stats(),
    M = S2#stats.clients,
    M = S2#stats.monitoring,
    M = N - 1.

test_locker_allocation(Child1, Child2) ->
    N = rpc(Child1, get_locker_id, []),
    % Validate a second call returns an identical value to the first
    N = rpc(Child1, get_locker_id, []),
    % Check the stats for a registered record
    S = lock_serv:stats(),
    2 = S#stats.clients,
    % But the second child should have a distinct value
    case rpc(Child2, get_locker_id, []) of
        N -> exit("Duplicate locker ID for second child");
        _ ->
            S2 = lock_serv:stats(),
            2 = S2#stats.clients,
            ok
    end.

test_stats(Child1, _Child2) ->
    S = lock_serv:stats(),
    ok = rpc(Child1, lock, ["key1"]),
    S2 = lock_serv:stats(),
    Sum = S#stats.locks + 1,
    Sum = S2#stats.locks.

drain_mailbox() ->
    case receive _M -> ok after 1 -> done end of
        ok -> drain_mailbox();
        done -> ok
    end.

run_test(F) ->
    gen_server:cast(lock_serv, reset),
    Child1 = spawn_link(?MODULE, child_loop, []),
    Child2 = spawn_link(?MODULE, child_loop, []),
    lists:foreach(fun(C) -> ok = rpc(C, set_timeout, [5]) end,
        [Child1, Child2]),
    F(Child1, Child2),
    lists:foreach(fun(C) -> C ! stop end, [Child1, Child2]),
    drain_mailbox().

tests() ->
    lists:foreach(fun run_test/1, [
        fun test_basic_lock/2,
        fun test_double_lock/2,
        fun test_delayed_lock/2,
        fun test_delayed_lock_release/2,
        fun test_delayed_lock_release_with_third_child/2,
        fun test_unlock_all/2,
        fun test_unlock_all_empty/2,
        fun test_delayed_unlock_all/2,
        fun test_dead_clients_hold_no_locks/2,
        fun test_dead_clients_lose_registration/2,
        fun test_locker_allocation/2,
        fun test_stats/2
        ]).

start() ->
    error_logger:info_msg("Running tests."),

    cover:start(),
    cover:compile("src/lock_serv", [{i, "include"}]),
    lock_serv:start_link(),
    tests(),
    {ok, CovRes} = cover:analyse_to_file(lock_serv, "cov.html", [html]),
    error_logger:info_msg("Coverage report:  ~p~n", [CovRes]),
    
    error_logger:info_msg("Tests complete~n").