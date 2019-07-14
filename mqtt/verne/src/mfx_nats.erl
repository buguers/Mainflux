-module(mfx_nats).
-behaviour(gen_server).
-export([
    start_link/0,
    init/1,
    publish/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

start_link() ->
    error_logger:info_msg("mfx_nats", []),

    [{_, NatsUrl}] = ets:lookup(mfx_cfg, nats_url),
    {ok, {_, _, NatsHost, NatsPort, _, _}} = http_uri:parse(NatsUrl),
    {ok, NatsConn} = nats:connect(list_to_binary(NatsHost), NatsPort, #{buffer_size => 10}),

    ets:insert(mfx_cfg, [
        {nats_conn, NatsConn}
    ]),

    % Start genserver for PUB
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

publish(Subject, Message) ->
    gen_server:cast(?MODULE, {publish, Subject, Message}).

handle_call(Name, _From, _State) ->
    Reply = lists:flatten(io_lib:format("hello ~s from mfx_nats genserver", [Name])),
    {reply, Reply, _State}.

handle_cast({publish, Subject, Message}, _State) ->
    [{nats_conn, Conn}] = ets:lookup(mfx_cfg, nats_conn),
    error_logger:info_msg("mfx_nats genserver cast ~p ~p ~p", [Subject, Conn, Message]),
    NewState = nats:pub(Conn, Subject, #{payload => Message}),
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    [].

init(_Args) ->
    error_logger:info_msg("mfx_nats genserver has started (~w)~n", [self()]),
    % If the initialization is successful, the function
    % should return {ok,State}, {ok,State,Timeout} ..
    {ok, nats_state}.
