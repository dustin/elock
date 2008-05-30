-module (lock_serv_test).

-export ([start/0, child_loop/0]).

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

drain_mailbox() ->
    case receive _M -> ok after 1 -> done end of
        ok -> drain_mailbox();
        done -> ok
    end.

run_test(F) ->
    gen_server:cast(lock_serv, reset),
    Child1 = spawn_link(?MODULE, child_loop, []),
    Child2 = spawn_link(?MODULE, child_loop, []),
    F(Child1, Child2),
    Child1 ! stop,
    Child2 ! stop,
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
        fun test_delayed_unlock_all/2
        ]).

start() ->
    error_logger:info_msg("Running tests."),

    cover:start(),
    cover:compile("src/lock_serv"),
    lock_serv:start_link(),
    tests(),
    {ok, CovRes} = cover:analyse_to_file(lock_serv, "cov.html", [html]),
    error_logger:info_msg("Coverage report:  ~p~n", [CovRes]),
    
    error_logger:info_msg("Tests complete~n").