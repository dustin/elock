{application, lock_serv,
    [{description, "The lock server."},
     {vsn, "1.0"},
     {modules, [lock_serv]},
     {registered, []},
     {applications, [kernel,stdlib]},
     {mod, {lock_serv_app, []}}
    ]}.
