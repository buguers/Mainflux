-module(mfx_grpc).
-behaviour(gen_server).
-export([
    start_link/0,
    init/1,
    send/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

start_link() ->
    error_logger:info_msg("mfx_grpc", []),

    ThingsUrl = case os:getenv("MF_THINGS_URL") of
        false -> "http://localhost:8181";
        ThingsEnv -> ThingsEnv
    end,

    {ok, {_, _, GrpcHost, GrpcPort, _, _}} = http_uri:parse(ThingsUrl),
    {ok, GrpcConn} = grpc_client:connect(tcp, GrpcHost, GrpcPort),

     ets:insert(mfx_cfg, [
        {things_url, ThingsUrl},
        {grpc_conn, GrpcConn}
    ]),

    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send(Method, Message) ->
    gen_server:call(?MODULE, {send, Method, Message}).

handle_call({send, Method, Message}, _From, _State) ->
    [{grpc_conn, Conn}] = ets:lookup(mfx_cfg, grpc_conn),
    {Status, Result} = case Method of
        identify ->
            internal_client:'IdentifyThing'(Conn, Message, []);
        can_access ->
            internal_client:'CanAccess'(Conn, Message, []);
        _ ->
            {error, wrong_method}
    end,

    case Status of
        ok ->
            #{
                grpc_status := 0,
                headers := #{<<":status">> := <<"200">>},
                http_status := HttpStatus,
                result :=
                    #{value := ThingId},
                status_message := <<>>,
                trailers := #{<<"grpc-status">> := <<"0">>}
            } = Result,

            case HttpStatus of
                200 ->
                    {reply, {ok, ThingId}, ok};
                _ ->
                    {reply, {error, HttpStatus}, error}
            end;
        _ ->
            {reply, {error, Status}, error}
    end.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    [].

init(_Args) ->
    error_logger:info_msg("mfx_grpc has started (~w)~n", [self()]),
    % If the initialization is successful, the function
    % should return {ok,State}, {ok,State,Timeout} ..
    {ok, grpc_state}.