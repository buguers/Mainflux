-module(mfx_auth_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    % Put ENV variables in ETS
    ets:new(mfx_cfg, [set, named_table, public]),

    % Start the process
    {ok, Sup} = mfx_auth_sup:start_link(),
    mfx_auth:load(application:get_all_env()),
    {ok, Sup}.

stop(_State) ->
    mfx_auth:unload().
