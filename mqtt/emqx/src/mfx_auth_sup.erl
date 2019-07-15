-module(mfx_auth_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_all, 1, 0}, [
        {mfx_grpc, {mfx_grpc, start_link, []}, permanent, 2000, worker, [mfx_grpc]},
        {mfx_nats, {mfx_nats, start_link, []}, permanent, 2000, worker, [mfx_nats]}
    ]} }.

