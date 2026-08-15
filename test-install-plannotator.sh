#!/bin/bash
set -euo pipefail

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
installer="$script_dir/install-plannotator.sh"
sandbox="$(mktemp -d)"
trap 'rm -rf "$sandbox"' EXIT

mkdir -p "$sandbox/home" "$sandbox/mock-bin" "$sandbox/tmp"

cat > "$sandbox/mock-bin/curl" <<'MOCK_CURL'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_CURL_LOG"

url="${!#}"
output=""
while [ "$#" -gt 0 ]; do
    case "$1" in
        -o)
            output="$2"
            shift 2
            ;;
        *)
            shift
            ;;
    esac
done

payload='#!/bin/sh
exit 0
'
case "$url" in
    *Ataraxy-Labs/sem*|*/sem-*)
        exit 22
        ;;
    *.sha256)
        checksum="$(printf '%s' "$payload" | shasum -a 256 | awk '{print $1}')"
        printf '%s  plannotator\n' "$checksum"
        ;;
    */plannotator-*)
        [ -n "$output" ]
        printf '%s' "$payload" > "$output"
        ;;
    *)
        echo "Unexpected curl URL: $url" >&2
        exit 22
        ;;
esac
MOCK_CURL
chmod +x "$sandbox/mock-bin/curl"

cat > "$sandbox/mock-bin/pi" <<'MOCK_PI'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_PI_LOG"
MOCK_PI
chmod +x "$sandbox/mock-bin/pi"

cat > "$sandbox/mock-bin/git" <<'MOCK_GIT'
#!/bin/bash
set -euo pipefail
case "${1:-}" in
    clone)
        destination="${!#}"
        mkdir -p "$destination/apps/opencode-plugin/commands"
        printf '%s\n' 'mock OpenCode command' > "$destination/apps/opencode-plugin/commands/plannotator-review.md"
        ;;
    sparse-checkout)
        ;;
    *)
        echo "Unexpected git invocation: $*" >&2
        exit 1
        ;;
esac
MOCK_GIT
chmod +x "$sandbox/mock-bin/git"

# Avoid touching the real shared /tmp/jiti cache during this isolated test.
cat > "$sandbox/mock-bin/rm" <<'MOCK_RM'
#!/bin/bash
for arg in "$@"; do
    if [ "$arg" = "/tmp/jiti" ]; then
        exit 0
    fi
done
exec /bin/rm "$@"
MOCK_RM
chmod +x "$sandbox/mock-bin/rm"

env -i \
MOCK_CURL_LOG="$sandbox/curl.log" \
MOCK_PI_LOG="$sandbox/pi.log" \
HOME="$sandbox/home" \
TMPDIR="$sandbox/tmp" \
PATH="$sandbox/mock-bin:/usr/bin:/bin" \
SHELL="/bin/bash" \
PLANNOTATOR_SKIP_AGENT_TERMINAL_INSTALL=1 \
PLANNOTATOR_INSTALL_CALLDIFF=0 \
bash "$installer" \
    --version v9.9.9 \
    --non-interactive \
    --no-extras \
    --model-invocable none \
    --skip-codex \
    --skip-gemini \
    --skip-kiro \
    > "$sandbox/output.log" 2>&1

failures=0
if [ ! -x "$sandbox/home/.local/third-party/bin/plannotator" ]; then
    echo "FAIL: installer did not create ~/.local/third-party/bin/plannotator" >&2
    failures=$((failures + 1))
fi
if [ -e "$sandbox/home/.local/bin/plannotator" ]; then
    echo "FAIL: installer wrote the binary under ~/.local/bin" >&2
    failures=$((failures + 1))
fi
if grep -Eiq 'Ataraxy-Labs/sem|/sem-' "$sandbox/curl.log"; then
    echo "FAIL: installer attempted to download SEM" >&2
    failures=$((failures + 1))
fi
if ! grep -Fqx 'install npm:@plannotator/pi-extension' "$sandbox/pi.log"; then
    echo "FAIL: installer did not update the Pi extension" >&2
    failures=$((failures + 1))
fi
opencode_command="$sandbox/home/.config/opencode/commands/plannotator-review.md"
if [ ! -f "$opencode_command" ] || ! grep -Fqx 'mock OpenCode command' "$opencode_command"; then
    echo "FAIL: installer did not install the OpenCode command" >&2
    failures=$((failures + 1))
fi

if [ "$failures" -ne 0 ]; then
    exit 1
fi
printf 'PASS: custom destination, no SEM, Pi/OpenCode integrations preserved\n'
