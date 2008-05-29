-module (lock_serv).

-behaviour (gen_server).

-export([start_link/0, terminate/2, handle_info/2, code_change/3]).
-export([lock/1, unlock/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(lock_state, {locks=dict:new()}).

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

unlock(Key) ->
    gen_server:call(?MODULE, {unlock, Key}).

init(_Args) ->
    {ok, #lock_state{}}.

handle_call({lock, Key}, From, Locks) ->
    {Response, Locks2} = lock(Key, From, Locks),
    {reply, Response, Locks2};
handle_call({unlock, Key}, From, Locks) ->
    {Response, Locks2} = unlock(Key, From, Locks),
    {reply, Response, Locks2}.

handle_cast(X, Locks) ->
    error_logger:info_msg("Got a cast.  Why?  ~p~n", [X]),
    Locks.

% Actual lock handling

lock(Key, {From, _Something}, Locks) ->
    error_logger:info_msg("Locking from ~p~n", [From]),
    case dict:find(Key, Locks#lock_state.locks) of
        {ok, From} -> {ok, Locks};
        {ok, _Key} -> {locked, Locks};
        error ->
            {ok, Locks#lock_state{locks=dict:store(
                Key, From, Locks#lock_state.locks)}}
    end.

unlock(Key, {From, _Something}, Locks) ->
    case dict:find(Key, Locks#lock_state.locks) of
        {ok, From} ->
            {ok, Locks#lock_state{locks=dict:erase(
                Key, Locks#lock_state.locks)}};
        {ok, _Someone} -> {not_yours, Locks};
        _ -> {not_locked, Locks}
    end.
