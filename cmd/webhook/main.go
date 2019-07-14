//
// Copyright (c) 2018
// Mainflux
//
// SPDX-License-Identifier: Apache-2.0
//

package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"syscall"

	kitprometheus "github.com/go-kit/kit/metrics/prometheus"
	"github.com/mainflux/mainflux"
	"github.com/mainflux/mainflux/logger"
	webhook "github.com/mainflux/mainflux/mqtt/webhook"
	"github.com/mainflux/mainflux/mqtt/webhook/api"
	"github.com/mainflux/mainflux/mqtt/webhook/nats"
	thingsapi "github.com/mainflux/mainflux/things/api/auth/grpc"
	broker "github.com/nats-io/go-nats"
	stdprometheus "github.com/prometheus/client_golang/prometheus"
	"google.golang.org/grpc"
)

const (
	defClientTLS = "false"
	defCACerts   = ""
	defPort      = "8180"
	defLogLevel  = "error"
	defNatsURL   = broker.DefaultURL
	defThingsURL = "localhost:8181"
	envPort      = "MF_MQTT_WEBHOOK_PORT"
	envLogLevel  = "MF_MQTT_WEBHOOK_LOG_LEVEL"
	envNatsURL   = "MF_NATS_URL"
	envThingsURL = "MF_THINGS_URL"
)

type config struct {
	thingsURL string
	natsURL   string
	logLevel  string
	port      string
}

func main() {

	cfg := loadConfig()

	logger, err := logger.New(os.Stdout, cfg.logLevel)
	if err != nil {
		log.Fatalf(err.Error())
	}

	nc, err := broker.Connect(cfg.natsURL)
	if err != nil {
		logger.Error(fmt.Sprintf("Failed to connect to NATS: %s", err))
		os.Exit(1)
	}
	defer nc.Close()

	conn := connectToThings(cfg, logger)
	defer conn.Close()

	cc := thingsapi.NewClient(conn)
	pub := nats.NewMessagePublisher(nc)

	svc := webhook.New(pub)
	svc = api.LoggingMiddleware(svc, logger)
	svc = api.MetricsMiddleware(
		svc,
		kitprometheus.NewCounterFrom(stdprometheus.CounterOpts{
			Namespace: "mqtt_webhook",
			Subsystem: "api",
			Name:      "request_count",
			Help:      "Number of requests received.",
		}, []string{"method"}),
		kitprometheus.NewSummaryFrom(stdprometheus.SummaryOpts{
			Namespace: "mqtt_webhook",
			Subsystem: "api",
			Name:      "request_latency_microseconds",
			Help:      "Total duration of requests in microseconds.",
		}, []string{"method"}),
	)

	errs := make(chan error, 2)

	go func() {
		p := fmt.Sprintf(":%s", cfg.port)
		logger.Info(fmt.Sprintf("MQTT webhook service started on port %s", cfg.port))
		errs <- http.ListenAndServe(p, api.MakeHandler(svc, cc))
	}()

	go func() {
		c := make(chan os.Signal)
		signal.Notify(c, syscall.SIGINT)
		errs <- fmt.Errorf("%s", <-c)
	}()

	err = <-errs
	logger.Error(fmt.Sprintf("MQTT webhook terminated: %s", err))
}

func loadConfig() config {
	return config{
		thingsURL: mainflux.Env(envThingsURL, defThingsURL),
		natsURL:   mainflux.Env(envNatsURL, defNatsURL),
		logLevel:  mainflux.Env(envLogLevel, defLogLevel),
		port:      mainflux.Env(envPort, defPort),
	}
}

func connectToThings(cfg config, logger logger.Logger) *grpc.ClientConn {
	var opts []grpc.DialOption
	opts = append(opts, grpc.WithInsecure())
	conn, err := grpc.Dial(cfg.thingsURL, opts...)
	if err != nil {
		logger.Error(fmt.Sprintf("Failed to connect to things service: %s", err))
		os.Exit(1)
	}
	return conn
}
