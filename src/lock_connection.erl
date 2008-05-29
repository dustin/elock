-module (lock_connection).

-export ([lock/1]).

lock(Socket) ->
    process_flag(trap_exit, true),
    receive
        go_ahead ->
            inet:setopts(Socket, [{active, true}])
    end,
    error_logger:info_msg("Got a connection~n", []),
    loop(Socket, "").

% remove the handler and exit
lock_exit(_Reason) ->
    error_logger:info_msg("lock_exit:  deleting handler~n", []),
    exit(closed).

% Commands go here.
process_command(Socket, "lock", [Key]) ->
    case lock_serv:lock(Key) of
        ok ->
            error_logger:info_msg("Locked ~p~n", [Key]),
            send_response(Socket, 200, "Acquired");
        _ ->
            send_response(Socket, 404, "Unavailable")
    end;
process_command(Socket, "lock", [Key, WaitStr]) ->
    {WaitSecs, []} = string:to_integer(WaitStr),
    Wait = WaitSecs * 1000,
    error_logger:info_msg("Waiting ~p for ~p~n", [Wait, Key]),
    case lock_serv:lock(Key, Wait) of
        ok ->
            error_logger:info_msg("Locked ~p~n", [Key]),
            send_response(Socket, 200, "Acquired");
        delayed ->
            receive
                {acquiring, Key, From} ->
                    From ! {ack, self()},
                    receive
                        {acquired, Key} ->
                            send_response (Socket, 200, "Acquired")
                    end
                after Wait ->
                    send_response(Socket, 404, "Unavailable")
            end
    end;
process_command(Socket, "unlock", [Key]) ->
    case lock_serv:unlock(Key) of
        ok ->
            error_logger:info_msg("Unlocked ~p~n", [Key]),
            send_response(Socket, 200, "Unlocked");
        X  ->
            send_response(Socket, 404, io_lib:format("~p", [X]))
    end;
process_command(Socket, "echo", Args) ->
    send_response(Socket, 200, io_lib:format("~p", [Args]));
process_command(Socket, "quit", _Args) ->
    send_response(Socket, 200, "Hey, it was nice seeing you."),
    self() ! close;
process_command(Socket, Cmd, _Args) ->
    send_response(Socket, 400, io_lib:format("Unknown command: ~p", [Cmd])).

process_incoming(_Socket, Data, 0) ->
    Data;
process_incoming(Socket, Data, Pos) ->
    [Cmd|Args] = string:tokens(string:strip(string:substr(Data, 1, Pos-1)), " "),
    error_logger:info_msg("Got command:  ~p(~p)~n", [Cmd, Args]),
    process_command(Socket, Cmd, Args),
    "".

send_response(Socket, Status, Message) when list(Status) ->
    gen_tcp:send(Socket, [Status, " ", Message, <<13,10>>]);
send_response(Socket, Status, Message) when integer(Status) ->
    send_response(Socket, integer_to_list(Status), Message).

loop(Socket, IncomingData) ->
    error_logger:info_msg("Current data:  ~p~n", [IncomingData]),
    CurrentData = process_incoming(Socket, IncomingData,
        string:str(IncomingData, "\r\n")),
    receive
        % Inbound messages
        {tcp, Socket, Bytes} ->
            error_logger:error_msg("lock_serv: Received data:  ~p~n", [Bytes]),
            loop(Socket, CurrentData ++ Bytes);
        % Control messages
        {tcp_closed, Socket} ->
            error_logger:info_msg("lock_serv:  socket closed~n", []),
            lock_exit(closed);
        {tcp_error, Socket, Reason} ->
            error_logger:error_msg("lock_serv:  socket error:  ~p~n", [Reason]),
            gen_tcp:close(Socket),
            lock_exit(Reason);
        % This happens when we obtain a lock after having lost interest
        {acquiring, _Key, From} ->
			From ! nak,
			loop(Socket, CurrentData);
        % Deaths
        close ->
            gen_tcp:close(Socket),
            lock_exit("Close Requested");
        {'EXIT', _U, Why} ->
            error_logger:info_msg("lock_serv: exiting:  ~p~n", [Why]),
            gen_tcp:close(Socket),
            lock_exit(Why);
        % Unknown
        Unknown ->
            error_logger:error_msg("lock_serv: Unhandled message:  ~p~n", [Unknown]),
            loop(Socket, CurrentData)
    end.
