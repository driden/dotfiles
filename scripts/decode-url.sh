#!/usr/bin/env bash

decode-url() {
	echo -n "$1" | python3 -c "import sys; from urllib.parse import unquote; print(unquote(sys.stdin.read()));"
}
