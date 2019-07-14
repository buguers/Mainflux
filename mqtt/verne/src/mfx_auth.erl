-module(mfx_auth).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

-include("proto/message.hrl").

%% This file demonstrates the hooks you typically want to use
%% if your plugin deals with Authentication or Authorization.
%%
%% All it does is:
%%  - authenticate every user and write the log
%%  - authorize every PUBLISH and SUBSCRIBE and write it to the log
%%
%% You don't need to implement all of these hooks, just the one
%% needed for your use case.
%%
%% IMPORTANT:
%%  these hook functions run in the session context

identify(undefined) ->
    {error, undefined};
identify(Password) ->
    error_logger:info_msg("identify: ~p", [Password]),
    [{_, AuthUrl}] = ets:lookup(mfx_cfg, auth_url),
    URL = [AuthUrl, <<"/identify">>],
    ReqBody = jsone:encode(#{<<"token">> => Password}),
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    error_logger:info_msg("access: ~p", [URL]),
    {ok, Status, _, Ref} = hackney:request(post, URL, ReqHeaders, ReqBody),
    case Status of
        200 ->
            case hackney:body(Ref) of
                {ok, RespBody} ->
                    {[{<<"id">>, Id}]} = jsone:decode(RespBody, [{object_format, tuple}]),
                    {ok, Id};
                _ ->
                    error
            end;
        _ ->
            error
    end.

access(UserName, ChannelId) ->
    error_logger:info_msg("access: ~p ~p", [UserName, ChannelId]),
    Password = get(UserName),
    case Password of
        undefined ->
            {error, undefined};
        _ ->
            [{_, AuthUrl}] = ets:lookup(mfx_cfg, auth_url),
            URL = [AuthUrl, <<"/channels/">>, ChannelId, <<"/access">>],
            error_logger:info_msg("URL: ~p", [URL]),
            ReqBody = jsone:encode(#{<<"token">> => Password}),
            ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
            {ok, Status, _, Ref} = hackney:request(post, URL, ReqHeaders, ReqBody),
            case Status of
                200 ->
                    case hackney:body(Ref) of
                        {ok, RespBody} ->
                            {[{<<"id">>, Id}]} = jsone:decode(RespBody, [{object_format, tuple}]),
                            {ok, Id};
                        _ ->
                            error
                    end;
                _ ->
                    error
            end
    end.

auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    error_logger:info_msg("auth_on_register: ~p ~p ~p ~p ~p", [Peer, SubscriberId, UserName, Password, CleanSession]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> CONNECT is authenticated
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, [{ModifierKey, NewVal}...]} -> CONNECT is authenticated,
    %% but we might want to set some options used throughout the client session:
    %%      - {mountpoint, NewMountPoint::string}
    %%      - {clean_session, NewCleanSession::boolean}
    %% 4. return {error, invalid_credentials} -> CONNACK_CREDENTIALS is sent
    %% 5. return {error, whatever} -> CONNACK_AUTH is sent

    case identify(Password) of
        {ok, _} ->
            % Save Username:Password mapping in process dictionary
            put(UserName, Password),
            ok;
        _ ->
            error
    end.

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> PUBLISH is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, NewPayload::binary} -> PUBLISH is authorized, but we changed the payload
    %% 4. return {ok, [{ModifierKey, NewVal}...]} -> PUBLISH is authorized, but we might have changed different Publish Options:
    %%     - {topic, NewTopic::string}
    %%     - {payload, NewPayload::binary}
    %%     - {qos, NewQoS::0..2}
    %%     - {retain, NewRetainFlag::boolean}
    %% 5. return {error, whatever} -> auth chain is stopped, and message is silently dropped (unless it is a Last Will message)
    %%

    % Topic is list of binaries, ex: [<<"channels">>, <<"1">>, <<"messages">>, <<"subtopic_1">>, ...]
    [<<"channels">>, ChannelId, Suffix] = Topic,
    case access(UserName, ChannelId) of
        {ok, PublisherId} ->
            RawMessage = #'RawMessage'{
                'channel' = ChannelId,
                'publisher' = PublisherId,
                'protocol' = "mqtt",
                'payload' = Payload
            },

            error_logger:info_msg("SUFFIX: ~p", [Suffix]),
            case Suffix of
                <<"messages">> ->
                    Subject = [<<"channel.">>, ChannelId],
                    mfx_nats:publish(Subject, message:encode_msg(RawMessage)),
                    ok;
                [<<"messages">>, Subtopic] ->
                    Subject = [<<"channel.">>, ChannelId, <<".">>, string:join([[X] || X <- Subtopic], ".")],
                    mfx_nats:publish(Subject, message:encode_msg(RawMessage)),
                    ok;
                _ ->
                    error
            end;
        _ ->
            error
    end.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, ClientId, Topics]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> SUBSCRIBE is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {error, whatever} -> auth chain is stopped, and no SUBACK is sent

    [_, ChannelId, _] = _Topic,
    case access(UserName, ChannelId) of
        {ok, _} ->
            ok;
        _ ->
            {error, "SUB not authorized"}
    end.
