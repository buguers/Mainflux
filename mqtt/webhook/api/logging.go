// Copyright (c) Mainflux
// SPDX-License-Identifier: Apache-2.0

// +build !test

package api

import (
	"fmt"
	"time"

	"github.com/mainflux/mainflux"
	log "github.com/mainflux/mainflux/logger"
	"github.com/mainflux/mainflux/mqtt/webhook"
)

var _ webhook.Service = (*loggingMiddleware)(nil)

type loggingMiddleware struct {
	logger log.Logger
	svc    webhook.Service
}

// LoggingMiddleware adds logging facilities to the adapter.
func LoggingMiddleware(svc webhook.Service, logger log.Logger) webhook.Service {
	return &loggingMiddleware{logger, svc}
}

func (lm *loggingMiddleware) Register(thingID string) (err error) {
	defer func(begin time.Time) {
		message := fmt.Sprintf("Method register thing %s took %s to complete", thingID, time.Since(begin))
		if err != nil {
			lm.logger.Warn(fmt.Sprintf("%s with error: %s.", message, err))
			return
		}
		lm.logger.Info(fmt.Sprintf("%s without errors.", message))
	}(time.Now())

	return lm.svc.Register(thingID)
}

func (lm *loggingMiddleware) Publish(msg mainflux.RawMessage) (err error) {
	defer func(begin time.Time) {
		destChannel := msg.Channel
		if msg.Subtopic != "" {
			destChannel = fmt.Sprintf("%s.%s", destChannel, msg.Subtopic)
		}
		message := fmt.Sprintf("Method publish to channel %s took %s to complete", destChannel, time.Since(begin))
		if err != nil {
			lm.logger.Warn(fmt.Sprintf("%s with error: %s.", message, err))
			return
		}
		lm.logger.Info(fmt.Sprintf("%s without errors.", message))
	}(time.Now())

	return lm.svc.Publish(msg)
}

func (lm *loggingMiddleware) Subscribe(topic string) (err error) {
	defer func(begin time.Time) {
		message := fmt.Sprintf("Method subscribe thing %s took %s to complete", topic, time.Since(begin))
		if err != nil {
			lm.logger.Warn(fmt.Sprintf("%s with error: %s.", message, err))
			return
		}
		lm.logger.Info(fmt.Sprintf("%s without errors.", message))
	}(time.Now())

	return lm.svc.Subscribe(topic)
}
