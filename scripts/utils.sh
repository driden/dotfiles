#!/usr/bin/env bash

function random_alpha() {
	length=${1:-15}
	rand_string=$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9' | head -c "$length")

	printf "%s\n" "$rand_string"
}

function date_iso8601() {
	date -u +%Y-%m-%dT%H:%M:%SZ
}

function date_timestamp() {
	date -u +%s
}
