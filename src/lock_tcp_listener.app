{application, lock_tcp_listener,
    [{description, "TCP listener for a distributed lock server."},
     {vsn, "1.0"},
     {modules, [lock_tcp_listener, lock_connection]},
     {registered, []},
     {applications, [kernel,stdlib]},
     {mod, {lock_tcp_listener_app, []}}
    ]}.
