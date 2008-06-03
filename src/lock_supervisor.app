{application, lock_supervisor,
    [{description, "The lock server."},
     {vsn, "1.0"},
     {modules, [lock_supervisor_app, lock_serv,
                lock_tcp_listener, lock_connection]},
     {registered, [lock_serv]},
     {applications, [kernel,stdlib]},
     {mod, {lock_supervisor_app, []}}
    ]}.
