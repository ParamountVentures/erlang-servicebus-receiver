-module(receiver_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_receivers/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

% Starts an instance of this module
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

% Initialises the child specs for this supervisor
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    AChild = {'receiver', {'receiver', start_link, []},
          Restart, Shutdown, Type, ['receiver']},

    {ok, {SupFlags, [AChild]}}.

% Starts a number of supervised worker instances
start_receivers(NumberOfReceivers) ->
  io:format("Starting~p receivers~n", [NumberOfReceivers]),
  lists:map(fun(X) -> supervisor:start_child(receiver_sup, [X]) end, lists:seq(1, NumberOfReceivers)).
