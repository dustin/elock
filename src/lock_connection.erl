-module (lock_connection).

-export ([lock/2]).

-include ("lock_stats.hrl").

lock(Socket, Parent) ->
    process_flag(trap_exit, true),
    erlang:monitor(process, Parent),
    receive
        go_ahead ->
            inet:setopts(Socket, [{active, true}])
    end,
    error_logger:info_msg("Got a connection~n", []),
    loop(Socket, "").

% remove the handler and exit
lock_exit(_Reason) ->
    error_logger:info_msg("lock_exit~n", []),
    exit(closed).

lock_response(Socket, Key, R) ->
    case R of
        ok ->
            error_logger:info_msg("Locked ~p~n", [Key]),
            send_response(Socket, 200, "Acquired");
        _ ->
            send_response(Socket, 409, "Unavailable")
    end.

format_stats(Stats) ->
	Lines = [
		"STATS",
		io_lib:format("STAT clients ~p", [Stats#stats.clients]),
		io_lib:format("STAT locks ~p", [Stats#stats.locks]),
		io_lib:format("STAT monitoring ~p", [Stats#stats.monitoring]),
		"END"],
	string:join(Lines, [13,10]).

% Commands go here.
process_command(Socket, "lock", [Key]) ->
    lock_response(Socket, Key, lock_serv:lock(Key));
process_command(Socket, "lock", [Key, WaitStr]) ->
    {WaitSecs, []} = string:to_integer(WaitStr),
    lock_response(Socket, Key, lock_serv:lock(Key, WaitSecs * 1000));
process_command(Socket, "unlock", [Key]) ->
    case lock_serv:unlock(Key) of
        ok ->
            error_logger:info_msg("Unlocked ~p~n", [Key]),
            send_response(Socket, 200, "Unlocked");
        X  ->
            send_response(Socket, 403, io_lib:format("~p", [X]))
    end;
process_command(Socket, "unlock_all", []) ->
    lock_serv:unlock_all(),
    send_response(Socket, 200, "OK");
process_command(Socket, "echo", Args) ->
    send_response(Socket, 200, io_lib:format("~p", [Args]));
process_command(Socket, "stats", []) ->
    Stats = lock_serv:stats(),
    send_response(Socket, 200, format_stats(Stats));
process_command(Socket, "conn_id", []) ->
    send_response(Socket, 200, lock_serv:get_locker_id());
process_command(Socket, "conn_id", [Requested]) ->
    error_logger:info_msg("Wanting to set conn id to ~p", [Requested]),
    case lock_serv:set_locker_id(Requested) of
        ok -> send_response(Socket, 200, "conn id set");
        denied -> send_response(Socket, 403, "cannot set conn id")
    end;
process_command(Socket, "set_timeout", [MillisStr]) ->
    {Millis, []} = string:to_integer(MillisStr),
    ok = lock_serv:set_timeout(Millis),
    send_response(Socket, 200, "timeout set");
process_command(Socket, "quit", _Args) ->
    send_response(Socket, 200, "Hey, it was nice seeing you."),
    self() ! close;
process_command(Socket, Cmd, _Args) ->
    send_response(Socket, 400, io_lib:format("Unknown command: ~p", [Cmd])).

kill_whitey(S) ->
	string:strip(
		string:strip(
			string:strip(S, both, $\n), both, $\r), both, $\ ).

extract_line(Data) ->
	Pos = string:chr(Data, $\n),
	Line = kill_whitey(string:substr(Data, 1, Pos-1)),
	Rest = string:strip(string:substr(Data, Pos), left, $\n),
	{Line, Rest}.

process_incoming(_Socket, Data, false) ->
    {nomore, Data};
process_incoming(Socket, Data, true) ->
	{Line, Extra} = extract_line(Data),
    [Cmd|Args] = string:tokens(string:strip(Line), " "),
    error_logger:info_msg("Got command:  ~p(~p)~n", [Cmd, Args]),
    process_command(Socket, Cmd, Args),
    {more, Extra}.

process_incoming(Socket, Data) ->
	case process_incoming(Socket, Data, lists:member($\n, Data)) of
		{more, Data2} -> process_incoming(Socket, Data2);
		{nomore, Data2} -> Data2
	end.

send_response(Socket, Status, Message) when list(Status) ->
    gen_tcp:send(Socket, [Status, " ", Message, <<13,10>>]);
send_response(Socket, Status, Message) when integer(Status) ->
    send_response(Socket, integer_to_list(Status), Message).

loop(Socket, IncomingData) ->
    CurrentData = process_incoming(Socket, IncomingData),
    receive
        % Inbound messages
        {tcp, Socket, Bytes} ->
            loop(Socket, CurrentData ++ Bytes);
        % Control messages
        {tcp_closed, Socket} ->
            error_logger:info_msg("lock_serv:  socket closed~n", []),
            lock_exit(closed);
        {tcp_error, Socket, Reason} ->
            error_logger:error_msg("lock_serv:  socket error:  ~p~n", [Reason]),
            gen_tcp:close(Socket),
            lock_exit(Reason);
        % Deaths
        close ->
            lock_exit("Close Requested"),
            loop(Socket, IncomingData);
        {'EXIT', _U, Why} ->
            error_logger:info_msg("lock_serv: exiting:  ~p~n", [Why]),
            gen_tcp:close(Socket),
            lock_exit(Why);
        {'DOWN', _Ref, process, _Parent, Why} ->
            error_logger:error_msg("Parent died (~p), shutting down", [Why]),
            gen_tcp:close(Socket),
            lock_exit(Why);
        % Unknown
        Unknown ->
            error_logger:error_msg("lock_serv: Unhandled message:  ~p~n", [Unknown]),
            loop(Socket, CurrentData)
    end.
