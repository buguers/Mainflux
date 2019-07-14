// Copyright (c) Mainflux
// SPDX-License-Identifier: Apache-2.0

package api

type authRegisterReq struct {
	peerAddr     string `json:"peer_addr,omitempty"`
	peerPort     int    `json:"peer_port,omitempty"`
	username     string `json:"username,omitempty"`
	password     string `json:"password,omitempty"`
	mountpoint   string `json:"mountpoint,omitempty"`
	clientId     string `json:"client_id,omitempty"`
	cleanSession bool   `json:"clean_session,omitempty"`
}

type topics struct {
	topic string `json:"topic,omitempty"`
	qos   int    `json:"qos,omitempty"`
}

type authSubscribeReq struct {
	clientId   string   `json:"client_id,omitempty"`
	mountpoint string   `json:"mountpoint,omitempty"`
	username   string   `json:"username,omitempty"`
	topics     []topics `json:"topics,omitempty"`
}

type authPublishReq struct {
	username   string `json:"username,omitempty"`
	clientId   string `json:"client_id,omitempty"`
	mountpoint string `json:"mountpoint,omitempty"`
	qos        int    `json:"qos,omitempty"`
	topic      string `json:"topic,omitempty"`
	payload    string `json:"payload,omitempty"`
	retain     bool   `json:"retain,omitempty"`
}
