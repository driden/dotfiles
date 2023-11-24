#!/usr/bin/env bash

function random_alpha() {
	length=${1:-15}
	rand_string=$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9' | head -c "$length")

	printf "%s\n" "$rand_string"
}
