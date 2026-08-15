#!/bin/bash
set -euo pipefail

script_dir="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
installer="$script_dir/install-plannotator.sh"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

failures=0
fail() {
    echo "FAIL: $*" >&2
    failures=$((failures + 1))
}

setup_case() {
    local name="$1"
    case_dir="$test_root/$name"
    mkdir -p \
        "$case_dir/home" \
        "$case_dir/home/.gemini" \
        "$case_dir/home/.kiro" \
        "$case_dir/home/.cache/opencode/packages/@plannotator" \
        "$case_dir/home/.bun/install/cache/@plannotator" \
        "$case_dir/home/.claude/plugins/marketplaces/plannotator/apps/hook/hooks" \
        "$case_dir/home/.claude/skills/core" \
        "$case_dir/home/.claude/skills/plannotator-review" \
        "$case_dir/home/.claude/skills/plannotator-archive" \
        "$case_dir/home/.claude/commands" \
        "$case_dir/home/.agents/skills/plannotator-review" \
        "$case_dir/home/.agents/skills/plannotator-archive" \
        "$case_dir/mock-bin" \
        "$case_dir/tmp"

    printf 'original Claude hook\n' > "$case_dir/home/.claude/plugins/marketplaces/plannotator/apps/hook/hooks/hooks.json"
    printf 'keep\n' > "$case_dir/home/.claude/skills/core/sentinel"
    printf '%s\n' 'disable-model-invocation: true' 'existing Claude skill' > "$case_dir/home/.claude/skills/plannotator-review/SKILL.md"
    printf '%s\n' 'disable-model-invocation: true' 'existing Codex skill' > "$case_dir/home/.agents/skills/plannotator-review/SKILL.md"
    printf 'keep\n' > "$case_dir/home/.claude/skills/plannotator-archive/sentinel"
    printf 'keep\n' > "$case_dir/home/.agents/skills/plannotator-archive/sentinel"
    printf 'keep\n' > "$case_dir/home/.claude/commands/plannotator-review.md"
    printf 'keep\n' > "$case_dir/home/.gemini/sentinel"
    printf 'keep\n' > "$case_dir/home/.kiro/sentinel"
    printf 'keep\n' > "$case_dir/home/.cache/opencode/packages/@plannotator/sentinel"
    printf 'keep\n' > "$case_dir/home/.bun/install/cache/@plannotator/sentinel"
    : > "$case_dir/curl.log"
    : > "$case_dir/pi.log"
    : > "$case_dir/rm.log"
    : > "$case_dir/npx.log"

    cat > "$case_dir/mock-bin/curl" <<'MOCK_CURL'
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

    cat > "$case_dir/mock-bin/git" <<'MOCK_GIT'
#!/bin/bash
set -euo pipefail
case "${1:-}" in
    clone)
        destination="${!#}"
        mkdir -p "$destination/apps/opencode-plugin/commands"
        printf '%s\n' 'mock OpenCode command' > "$destination/apps/opencode-plugin/commands/plannotator-review.md"
        for scope in claude core; do
            for skill in plannotator-review plannotator-annotate plannotator-last; do
                mkdir -p "$destination/apps/skills/$scope/$skill"
                printf '%s\n' "mock $scope $skill" > "$destination/apps/skills/$scope/$skill/SKILL.md"
            done
        done
        ;;
    sparse-checkout)
        ;;
    *)
        echo "Unexpected git invocation: $*" >&2
        exit 1
        ;;
esac
MOCK_GIT

    cat > "$case_dir/mock-bin/pi" <<'MOCK_PI'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_PI_LOG"
MOCK_PI

    cat > "$case_dir/mock-bin/codex" <<'MOCK_CODEX'
#!/bin/bash
exit 0
MOCK_CODEX

    cat > "$case_dir/mock-bin/npx" <<'MOCK_NPX'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_NPX_LOG"
exit 99
MOCK_NPX

    # Log removals and protect the real shared /tmp/jiti cache.
    cat > "$case_dir/mock-bin/rm" <<'MOCK_RM'
#!/bin/bash
set -euo pipefail
printf '%s\n' "$*" >> "$MOCK_RM_LOG"
for arg in "$@"; do
    if [ "$arg" = "/tmp/jiti" ]; then
        exit 0
    fi
done
exec /bin/rm "$@"
MOCK_RM

    chmod +x "$case_dir/mock-bin/"*
}

run_case() {
    set +e
    env -i \
        MOCK_CURL_LOG="$case_dir/curl.log" \
        MOCK_PI_LOG="$case_dir/pi.log" \
        MOCK_RM_LOG="$case_dir/rm.log" \
        MOCK_NPX_LOG="$case_dir/npx.log" \
        HOME="$case_dir/home" \
        TMPDIR="$case_dir/tmp" \
        PATH="$case_dir/mock-bin:/usr/bin:/bin" \
        SHELL="/bin/bash" \
        PLANNOTATOR_SKIP_AGENT_TERMINAL_INSTALL=1 \
        PLANNOTATOR_INSTALL_CALLDIFF=0 \
        bash "$installer" \
            --version v9.9.9 \
            --non-interactive \
            --no-extras \
            --model-invocable none \
            "$@" \
            > "$case_dir/output.log" 2>&1
    case_status=$?
    set -e
}

assert_common_install_policy() {
    if [ ! -x "$case_dir/home/.local/third-party/bin/plannotator" ]; then
        fail "$1 did not install ~/.local/third-party/bin/plannotator"
    fi
    if [ -e "$case_dir/home/.local/bin/plannotator" ]; then
        fail "$1 wrote the binary under ~/.local/bin"
    fi
    if grep -Eiq 'Ataraxy-Labs/sem|/sem-' "$case_dir/curl.log"; then
        fail "$1 attempted to download SEM"
    fi
    if [ -s "$case_dir/npx.log" ]; then
        fail "$1 invoked the unscoped global skills installer"
    fi
}

assert_excluded_common_harnesses_untouched() {
    local label="$1"
    if [ ! -f "$case_dir/home/.gemini/sentinel" ] || [ -e "$case_dir/home/.gemini/policies/plannotator.toml" ]; then
        fail "$label modified Gemini"
    fi
    if [ ! -f "$case_dir/home/.kiro/sentinel" ] || [ -e "$case_dir/home/.kiro/agents/plannotator.json" ]; then
        fail "$label modified Kiro"
    fi
}

# Minimal remains a deliberate binary-only path and needs no selector.
setup_case minimal
run_case --minimal
if [ "$case_status" -ne 0 ]; then
    fail "--minimal install failed"
else
    assert_common_install_policy minimal
fi
if [ -s "$case_dir/pi.log" ] || [ -e "$case_dir/home/.config/opencode/commands/plannotator-review.md" ]; then
    fail "minimal install wrote harness integrations"
fi

# Current machine: OpenCode and OpenCode 2 share one config; Pi is separate.
setup_case opencode-pi
run_case --skip-claude --skip-codex --skip-gemini --skip-kiro --extras --model-invocable plannotator-review
if [ "$case_status" -ne 0 ]; then
    fail "opencode,pi install failed"
else
    assert_common_install_policy opencode-pi
fi
if ! grep -Fqx 'install npm:@plannotator/pi-extension' "$case_dir/pi.log"; then
    fail "opencode,pi did not update Pi"
fi
opencode_command="$case_dir/home/.config/opencode/commands/plannotator-review.md"
if [ ! -f "$opencode_command" ] || ! grep -Fqx 'mock OpenCode command' "$opencode_command"; then
    fail "opencode,pi did not install the shared OpenCode/OpenCode2 command"
fi
if [ -e "$case_dir/home/.codex" ] || ! grep -Fqx 'existing Codex skill' "$case_dir/home/.agents/skills/plannotator-review/SKILL.md"; then
    fail "opencode,pi modified Codex"
fi
if ! grep -Fqx 'existing Claude skill' "$case_dir/home/.claude/skills/plannotator-review/SKILL.md" || ! grep -Fqx 'original Claude hook' "$case_dir/home/.claude/plugins/marketplaces/plannotator/apps/hook/hooks/hooks.json"; then
    fail "opencode,pi modified Claude"
fi
if [ ! -f "$case_dir/home/.claude/skills/core/sentinel" ] || [ ! -f "$case_dir/home/.claude/skills/plannotator-archive/sentinel" ] || [ ! -f "$case_dir/home/.agents/skills/plannotator-archive/sentinel" ] || [ ! -f "$case_dir/home/.claude/commands/plannotator-review.md" ]; then
    fail "opencode,pi cleaned an excluded harness"
fi
if ! grep -q '^disable-model-invocation: true$' "$case_dir/home/.claude/skills/plannotator-review/SKILL.md" || ! grep -q '^disable-model-invocation: true$' "$case_dir/home/.agents/skills/plannotator-review/SKILL.md"; then
    fail "opencode,pi changed model invocation in an excluded harness"
fi
if [ -e "$case_dir/home/.cache/opencode/packages/@plannotator/sentinel" ] || [ -e "$case_dir/home/.bun/install/cache/@plannotator/sentinel" ]; then
    fail "opencode,pi did not refresh OpenCode caches"
fi
assert_excluded_common_harnesses_untouched opencode-pi

# Claude-only machine.
setup_case claude
run_case --skip-codex --skip-gemini --skip-kiro --skip-opencode --skip-pi
if [ "$case_status" -ne 0 ]; then
    fail "claude install failed"
else
    assert_common_install_policy claude
fi
if ! grep -Fqx 'mock claude plannotator-review' "$case_dir/home/.claude/skills/plannotator-review/SKILL.md"; then
    fail "claude did not install Claude skills"
fi
if grep -Fqx 'original Claude hook' "$case_dir/home/.claude/plugins/marketplaces/plannotator/apps/hook/hooks/hooks.json"; then
    fail "claude did not refresh the installed Claude plugin hook"
fi
if [ -s "$case_dir/pi.log" ] || [ -e "$case_dir/home/.config/opencode/commands/plannotator-review.md" ]; then
    fail "claude modified Pi or OpenCode"
fi
if [ -e "$case_dir/home/.codex" ] || ! grep -Fqx 'existing Codex skill' "$case_dir/home/.agents/skills/plannotator-review/SKILL.md" || [ ! -f "$case_dir/home/.agents/skills/plannotator-archive/sentinel" ]; then
    fail "claude modified Codex"
fi
if [ ! -e "$case_dir/home/.cache/opencode/packages/@plannotator/sentinel" ] || [ ! -e "$case_dir/home/.bun/install/cache/@plannotator/sentinel" ]; then
    fail "claude cleared OpenCode caches"
fi
assert_excluded_common_harnesses_untouched claude

# Codex-only machine.
setup_case codex
run_case --skip-claude --skip-gemini --skip-kiro --skip-opencode --skip-pi
if [ "$case_status" -ne 0 ]; then
    fail "codex install failed"
else
    assert_common_install_policy codex
fi
if [ ! -f "$case_dir/home/.codex/hooks.json" ] || ! grep -Fqx 'mock core plannotator-review' "$case_dir/home/.agents/skills/plannotator-review/SKILL.md"; then
    fail "codex did not install hooks and shared skills"
fi
if [ -s "$case_dir/pi.log" ] || [ -e "$case_dir/home/.config/opencode/commands/plannotator-review.md" ]; then
    fail "codex modified Pi or OpenCode"
fi
if ! grep -Fqx 'existing Claude skill' "$case_dir/home/.claude/skills/plannotator-review/SKILL.md" || ! grep -Fqx 'original Claude hook' "$case_dir/home/.claude/plugins/marketplaces/plannotator/apps/hook/hooks/hooks.json" || [ ! -f "$case_dir/home/.claude/skills/core/sentinel" ] || [ ! -f "$case_dir/home/.claude/skills/plannotator-archive/sentinel" ] || [ ! -f "$case_dir/home/.claude/commands/plannotator-review.md" ]; then
    fail "codex modified Claude"
fi
if [ ! -e "$case_dir/home/.cache/opencode/packages/@plannotator/sentinel" ] || [ ! -e "$case_dir/home/.bun/install/cache/@plannotator/sentinel" ]; then
    fail "codex cleared OpenCode caches"
fi
assert_excluded_common_harnesses_untouched codex

if [ "$failures" -ne 0 ]; then
    exit 1
fi
printf 'PASS: destination/no-SEM policy and documented skip combinations verified\n'
