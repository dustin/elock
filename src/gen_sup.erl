%%
%% A really generic supervisor.
%%

-module(gen_sup).

-behavior(supervisor).

-export([init/1]).

% Generic supervisor initialization
init([Rv|_Args]) -> {ok, Rv}.
