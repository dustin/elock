-module (lock_serv_test).

-export ([start/0, child_loop/0]).

child_loop() ->
    receive
        {Sender, lock, [Thing]} ->
            Sender ! lock_serv:lock(Thing),
            child_loop();
        {Sender, unlock, [Thing]} ->
            Sender ! lock_serv:unlock(Thing),
            child_loop();
        stop -> ok
    end.

rpc(Child, Message, Args, Timeout) ->
    Child ! {self(), Message, Args},
    receive
        M -> M
        after Timeout ->
            exit(io_lib:format("Timed out waiting for ~p", [Message]))
    end.

rpc(Child, Message, Args) -> rpc(Child, Message, Args, 100).

test_basic_lock(Child1, Child2) ->
    ok = rpc(Child1, lock, ["test"]),
    locked = rpc(Child2, lock, ["test"]),
    not_yours = rpc(Child2, unlock, ["test"]),
    ok = rpc(Child1, unlock, ["test"]),
    not_locked = rpc(Child1, unlock, ["test"]).

run_test(F, Child1, Child2) ->
    gen_server:cast(lock_serv, reset),
    F(Child1, Child2).

tests(Child1, Child2) ->
    run_test(fun test_basic_lock/2, Child1, Child2).

start() ->
    error_logger:info_msg("Running tests."),
    lock_serv:start_link(),
    Child1 = spawn_link(?MODULE, child_loop, []),
    Child2 = spawn_link(?MODULE, child_loop, []),

    tests(Child1, Child2),
    
    error_logger:info_msg("Tests complete~n").