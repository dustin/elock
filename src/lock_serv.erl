-module (lock_serv).

-behaviour (gen_server).

-export([start_link/0, terminate/2, handle_info/2, code_change/3]).
-export([lock/1, lock/2, unlock/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(lock_state, {
    % Currently held locks (key -> pid)
    locks=dict:new(),
    % Current clients waiting for lock (key -> queue<pid>)
    waiters=dict:new()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

terminate(shutdown, _State) ->
    ok.

handle_info({'EXIT', Pid, Reason}, State) ->
    error_logger:info_msg("Got an exit from ~p: ~p~n", [Pid, Reason]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("Code's changing.  Hope that's OK~n", []),
    {ok, State}.

lock(Key) ->
    gen_server:call(?MODULE, {lock, Key}).

lock(Key, Wait) ->
    gen_server:call(?MODULE, {lock, Key, Wait}).

unlock(Key) ->
    gen_server:call(?MODULE, {unlock, Key}).

init(_Args) ->
    {ok, #lock_state{}}.

handle_call({lock, Key}, From, Locks) ->
    {Response, Locks2} = lock(Key, From, Locks),
    {reply, Response, Locks2};
handle_call({lock, Key, Wait}, From, Locks) ->
    {Response, Locks2} = lock(Key, Wait, From, Locks),
    {reply, Response, Locks2};
handle_call({unlock, Key}, From, Locks) ->
    {Response, Locks2} = unlock(Key, From, Locks),
    {reply, Response, Locks2}.

handle_cast(X, Locks) ->
    error_logger:info_msg("Got a cast.  Why?  ~p~n", [X]),
    Locks.

% Actual lock handling

lock(Key, {From, _Something}, Locks) ->
    case dict:find(Key, Locks#lock_state.locks) of
        {ok, From} -> {ok, Locks};
        {ok, _Key} -> {locked, Locks};
        error -> {ok, unconditional_lock(Key, From, Locks)}
    end.

lock(Key, Wait, {From, Something}, Locks) ->
    case lock(Key, {From, Something}, Locks) of
        {ok, Rv} -> {ok, Rv};
        _ -> {delayed, enqueue_waiter(Key, Wait, From, Locks)}
    end.

unlock(Key, {From, _Something}, Locks) ->
    case dict:find(Key, Locks#lock_state.locks) of
        {ok, From} ->
            {ok, hand_over_lock(Key, Locks)};
        {ok, _Someone} -> {not_yours, Locks};
        _ -> {not_locked, Locks}
    end.

% Private support stuff

% Reserve the lock
unconditional_lock(Key, From, Locks) ->
    Locks#lock_state{locks=dict:store(Key, From, Locks#lock_state.locks)}.

% return the specified lock.  If someone else wants it, give it up
hand_over_lock(Key, Locks) ->
    case dict:find(Key, Locks#lock_state.waiters) of
        {ok, Q} ->
            try_waiter(Key, Q, Locks);
        error ->
            Locks#lock_state{locks=dict:erase(Key, Locks#lock_state.locks)}
    end.

try_waiter(Key, Q, Locks) ->
    case queue:is_empty(Q) of
    true ->
        Locks#lock_state{locks=dict:erase(Key, Locks#lock_state.locks)};
    _ ->
        {{value, Waiter}, Q2} = queue:out(Q),
        Waiter ! {acquiring, Key, self()},
        receive
            {ack, Waiter} ->
                Waiter ! {acquired, Key},
                unconditional_lock(Key, Waiter,
                    Locks#lock_state{
                        waiters=dict:store(Key, Q2, Locks#lock_state.waiters)});
            _ -> try_waiter(Key, Q2, Locks)
            after 25 ->
                try_waiter(Key, Q2, Locks)
        end
    end.

% I may want to have this thing magically time out after a while.
enqueue_waiter(Key, _Wait, From, Locks) ->
    Q = case dict:find(Key, Locks#lock_state.waiters) of
        {ok, Queue} -> Queue;
        error -> queue:new()
    end,
    Locks#lock_state{waiters=dict:store(
        Key, queue:in(From, Q), Locks#lock_state.waiters)}.