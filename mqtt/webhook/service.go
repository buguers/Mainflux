// Copyright (c) Mainflux
// SPDX-License-Identifier: Apache-2.0

package webhook

import (
	"errors"

	"github.com/mainflux/mainflux"
)

var (
	// ErrConflict indicates usage of the existing email during account
	// registration.
	ErrConflict = errors.New("email already taken")

	// ErrMalformedEntity indicates malformed entity specification (e.g.
	// invalid username or password).
	ErrMalformedEntity = errors.New("malformed entity specification")

	// ErrUnauthorizedAccess indicates missing or invalid credentials provided
	// when accessing a protected resource.
	ErrUnauthorizedAccess = errors.New("missing or invalid credentials provided")

	// ErrNotFound indicates a non-existent entity request.
	ErrNotFound = errors.New("non-existent entity")
)

// Service specifies an API that must be fullfiled by the domain service
// implementation, and all of its decorators (e.g. logging & metrics).
type Service interface {
	// Register implements onRegister() hook
	Register(thingID string) error

	// Publish implements onPublish() hook
	Publish(msg mainflux.RawMessage) error

	// Subscribe implements onSubscribe hook
	Subscribe() error
}

var _ Service = (*webhookService)(nil)

type webhookService struct {
	pub mainflux.MessagePublisher
}

// New instantiates the HTTP webhook implementation.
func New(pub mainflux.MessagePublisher) Service {
	return &webhookService{pub}
}

func (svc *webhookService) Register(thingID string) error {
	return nil
}

func (svc *webhookService) Publish(msg mainflux.RawMessage) error {
	return svc.pub.Publish(msg)
}

func (svc *webhookService) Subscribe() error {
	return nil
}
