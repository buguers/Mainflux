# MQTT adapter

MQTT adapter provides an MQTT API for sending and receiving messages through the
platform.

## Configuration

The service is configured using the environment variables presented in the
following table. Note that any unset variables will be replaced with their
default values.

| Variable             | Description         | Default               |
|----------------------|---------------------|-----------------------|
| MF_MQTT_ADAPTER_PORT | Service MQTT port   | 1883                  |
| MF_MQTT_WS_PORT      | WebSocket port      | 8880                  |
| MF_NATS_URL          | NATS instance URL   | nats://localhost:4222 |
| MF_THINGS_URL        | Things service URL  | localhost:8181        |

## Deployment

### Prepare
Install [gpb](https://github.com/tomas-abrahamsson/gpb)
```
git clone https://github.com/tomas-abrahamsson/gpb.git
cd gpb
git checkout 4.9.0
make -j 16
```

Then generate Erlang proto files:
```
mkdir -p ./src/proto
./gpb/bin/protoc-erl -I ./gpb/ ../*.proto -o ./src/proto
cp ./gpb/include/gpb.hrl ./src/proto/
```

```
git clone https://github.com/Bluehouse-Technology/grpc_client.git
cd grpc_client && make -j 16
make shell
```
Then in Erlang shell:
```
1> grpc_client:compile("../../internal.proto").
```

Outside of shell:
```
mv ./internal_client.erl ../src/proto
```

### Compile
```
./rebar3 compile
```

### Load Plugin

First start VerneMQ broker:
```
cd $VERNEMQ_BROKER_PATH
./_build/default/rel/vernemq/bin/vernemq start
```

Remove other plugins:
```
cd $VERNEMQ_BROKER_PATH
./_build/default/rel/vernemq/bin/vmq-admin plugin disable -n vmq_passwd
./_build/default/rel/vernemq/bin/vmq-admin plugin disable -n vmq_acl
```

Enable Mainflux `mfx_auth` plugin:
```
cd $VERNEMQ_BROKER_PATH
./_build/default/rel/vernemq/bin/vmq-admin plugin enable -n mfx_auth -p <path_to_mfx_auth_plugin>/_build/default
```

## Debugging
Inspect logs:
```
cd $VERNEMQ_BROKER_PATH
cat _build/default/rel/vernemq/log/console.log
cat _build/default/rel/vernemq/log/error.log
```


