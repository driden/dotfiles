#!/usr/bin/env bash

lll() {
	exa --long
}

tt() {
	exa --tree --level="${1:=1}"
}
