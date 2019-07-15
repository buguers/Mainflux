// Copyright (c) Mainflux
// SPDX-License-Identifier: Apache-2.0

package api

import (
	"context"

	"github.com/go-kit/kit/endpoint"
	"github.com/mainflux/mainflux"
	"github.com/mainflux/mainflux/mqtt/webhook"
)

func authRegisterEndpoint(svc webhook.Service) endpoint.Endpoint {
	return func(_ context.Context, request interface{}) (interface{}, error) {
		req := request.(authRegisterReq)
		err := svc.Register(req.username)
		return nil, err
	}
}

func authPublishEndpoint(svc webhook.Service) endpoint.Endpoint {
	return func(_ context.Context, request interface{}) (interface{}, error) {
		msg := request.(mainflux.RawMessage)
		err := svc.Publish(msg)
		return nil, err
	}
}

func authSubscribeEndpoint(svc webhook.Service) endpoint.Endpoint {
	return func(_ context.Context, request interface{}) (interface{}, error) {
		req := request.(authSubscribeReq)
		err := svc.Subscribe(req.topics[0].topic)
		return nil, err
	}
}
