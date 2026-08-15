# Plannotator Agent Review Tools Design

**Date:** 2026-08-15
**Status:** Proposed
**Scope:** Global Pi and OpenCode integration

## Context

Plannotator already provides user commands and managed plan-review tools, but its arbitrary-document and code-diff commands are not model-callable awaited tools in both Pi and OpenCode. In Pi, the slash-command review flow opens asynchronously and later injects a follow-up message. Generic Agent Skills can run the foreground CLI, but OpenCode's Bash timeouts make long human review sessions unreliable.

The desired interaction is:

- Agents offer visual review after creating or substantially revising reviewable Markdown artifacts such as specifications, plans, designs, proposals, and drafts.
- Review is optional until the user accepts the offer.
- Once accepted, Plannotator stays attached to the originating tool call. Submitted feedback returns as that tool result, and the agent resumes without polling or another user prompt.
- After feedback, the agent revises and automatically reopens review until approval, dismissal, or cancellation.
- Code-diff review is a separate workflow started only when the user requests it. Once started, feedback/fix/re-review also loops automatically.
- Pi and OpenCode expose equivalent tools globally.

## Goals

1. Add two global, model-callable tools to Pi and OpenCode:
   - `plannotator_review_document`
   - `plannotator_review_diff`
2. Keep every browser review in the foreground with no review timeout.
3. Return the human decision through the original tool result.
4. Support document review for a repository-local Markdown file.
5. Support code review targets for:
   - the current repository state;
   - a full GitHub PR or GitLab MR URL;
   - a bare PR/MR number resolved from the repository remote;
   - `<base>..HEAD` and `<base>...HEAD`.
6. Rebuild range reviews from the selected base through the current working tree every time, including committed, staged, unstaged, untracked, deleted, renamed, and binary changes.
7. Keep host adapters shallow and put target parsing, Git preparation, process lifecycle, and result handling behind one shared module interface.
8. Deploy through the dotfiles repository's existing GNU Stow workflow.

## Non-goals

- Automatically review every code edit.
- Make document review mandatory before an agent may stop.
- Support arbitrary `A..B` or `A...B` ranges in the first version.
- Support bare branch names as implicit ranges; callers must use `<base>..HEAD` or `<base>...HEAD`.
- Replace Plannotator's existing plan mode, slash commands, or Agent Skills.
- Add Claude Code or Codex native tools in the first version.
- Modify the in-progress `install-plannotator.sh` or `test-install-plannotator.sh` work.
- Publish an npm package or introduce another executable wrapper.

## Tracked layout and Stow deployment

Track files at their final home-relative locations:

```text
.local/lib/plannotator-agent-tools.mjs
.pi/agent/extensions/plannotator-tools.ts
.pi/agent/AGENTS.md
.config/opencode/plugins/plannotator-tools.ts
.config/opencode/AGENTS.md
tests/plannotator-agent-tools.test.mjs
```

`./link.sh` uses GNU Stow to expose them as:

```text
~/.local/lib/plannotator-agent-tools.mjs
~/.pi/agent/extensions/plannotator-tools.ts
~/.pi/agent/AGENTS.md
~/.config/opencode/plugins/plannotator-tools.ts
~/.config/opencode/AGENTS.md
```

The repository currently ignores `.config/opencode`; change `.gitignore` narrowly so only the Plannotator plugin and global instruction file are tracked while OpenCode's generated state, package files, caches, and unrelated plugins remain ignored.

Add the top-level test directory to `.stow-local-ignore` so tests are not linked into `$HOME`. Preserve all existing uncommitted changes in `.stow-local-ignore` and the installer scripts.

The two adapter paths and the shared module path have the same three-level relationship to the repository/home root, so both adapters can import:

```text
../../../.local/lib/plannotator-agent-tools.mjs
```

That relative import resolves correctly both in the repository and after Stow linking. There is no custom symlink installer and no additional executable.

## Module design

### Shared module

`.local/lib/plannotator-agent-tools.mjs` is the deep module. Its interface is intentionally small:

```js
reviewDocument({ cwd, path, signal })
reviewDiff({ cwd, target, signal })
```

The interface returns a normalized textual result suitable for direct delivery as a model tool result. Internally the module owns:

- closed-grammar target parsing;
- path and repository validation;
- PR/MR number resolution;
- Git revision resolution;
- complete working-tree snapshot materialization for range review;
- foreground child-process execution;
- stdout/stderr capture;
- cancellation and process-tree cleanup;
- temporary worktree/data-directory cleanup;
- document JSON decision parsing;
- actionable error messages.

The module invokes programs with argument arrays, never interpolated shell strings.

### Pi adapter

`.pi/agent/extensions/plannotator-tools.ts` registers the two tools using `pi.registerTool()`. It maps Pi's `ctx.cwd` and tool `AbortSignal` into the shared module and translates the completed text or error into Pi's tool-result shape.

It does not use Pi's background slash-command path. It does not return a pending review identifier. The `execute` promise remains unresolved until review finishes or is cancelled.

### OpenCode adapter

`.config/opencode/plugins/plannotator-tools.ts` exports two plugin-defined tools. It maps OpenCode's `context.directory`/worktree and abort signal into the same shared module, then returns the completed text.

It does not call OpenCode's bounded Bash tool. The plugin owns the foreground child process, so a human review may remain open beyond Bash's normal timeout.

## Tool interfaces

### `plannotator_review_document`

Input:

```ts
{
  path: string
}
```

Rules:

- Resolve relative paths against the active repository directory.
- Require an existing regular `.md` or `.mdx` file inside the active working directory.
- Reject an empty file before opening the browser.
- Invoke:

  ```text
  plannotator annotate <absolute-path> --gate --json
  ```

- Parse the single JSON decision and return one of:
  - approved, optionally with notes;
  - annotated, with feedback;
  - dismissed.

The global instruction policy—not the tool implementation—controls the offer and automatic revision loop.

### `plannotator_review_diff`

Input:

```ts
{
  target?: string | number
}
```

Closed target grammar:

- omitted or `"current"`: current repository review;
- complete supported HTTP(S) GitHub PR/GitLab MR URL;
- positive integer or `#<positive-integer>`: bare PR/MR number;
- exactly `<base>..HEAD` or `<base>...HEAD` with a non-empty base.

Reject every other value. In particular, never pass an unsupported positional to `plannotator review`, because Plannotator silently interprets unsupported values as local review.

#### Current repository

Invoke `plannotator review` in the active working directory and rely on Plannotator's VCS auto-detection.

#### Full PR/MR URL

Validate the URL shape, then invoke `plannotator review <url>`.

#### Bare PR/MR number

Resolve repository remotes without shell interpolation. If exactly one repository/forge is unambiguous, construct a full URL:

- GitHub: `https://<host>/<owner>/<repo>/pull/<number>`
- GitLab: `https://<host>/<group>/<repo>/-/merge_requests/<number>`

Fail with an actionable request for a full URL when remotes are absent, ambiguous, or an enterprise host cannot be classified safely. Never silently prefer a fork's `origin` over a distinct `upstream`.

#### Base range

Accept only two forms:

- `<base>..HEAD`: direct base-to-working-tree review;
- `<base>...HEAD`: merge-base-to-working-tree review.

Resolve the base with `git rev-parse --verify --end-of-options <base>^{commit}`. For triple-dot, resolve the merge base against `HEAD`. Use the resulting immutable commit as the snapshot base.

Each invocation constructs a fresh isolated review workspace representing the entire delta from that base through the real current working tree. It must include:

- commits after the base;
- staged changes;
- unstaged changes;
- untracked files;
- deletions and renames;
- binary changes.

Run Plannotator in that isolated workspace in a mode that displays the materialized delta as local changes. Always remove temporary worktrees, indexes, patches, and Plannotator data directories in `finally`, including startup failures and cancellation.

Reopening after feedback reruns materialization from scratch, so the next review includes the complete updated delta rather than only the latest fix.

## Agent policy

Install equivalent concise global instructions for Pi and OpenCode:

1. After creating or substantially revising a coherent spec, plan, design, proposal, or draft in Markdown, offer to open that artifact in Plannotator.
2. Do not interrupt incremental drafting on every write; offer when a coherent reviewable version exists.
3. If the user declines, continue normally.
4. If the user accepts, call `plannotator_review_document` and wait.
5. On annotation feedback, revise the same file and automatically call the tool again without asking.
6. End the loop only on approval, dismissal, cancellation, or an unrecoverable tool error.
7. Do not automatically start code review merely because code changed.
8. When the user requests code review, call `plannotator_review_diff` with the requested target.
9. After code feedback, fix and verify the complete change, then automatically rerun the logical review target with all fixes included. For a range, regenerate base-through-working-tree state. For a PR/MR target, never push automatically: if fixes are already remote, review the URL again; otherwise, when the active checkout matches the PR/MR head, review the hosted change's base through the local working tree. If the checkout cannot be matched safely, stop with an actionable explanation rather than rereviewing stale remote content.

These instructions guide model behavior; they are not represented as mandatory lifecycle hooks.

## Process lifecycle and cancellation

- Start Plannotator as a foreground child with no timeout.
- Keep the host tool promise pending until the child exits.
- Stream no partial decision as a successful result.
- On host cancellation, terminate the Plannotator process and descendants, await cleanup, and return/throw the host-appropriate cancellation result.
- Treat nonzero exits, malformed document JSON, missing binary, Git failures, browser startup failures, and cleanup failures as errors rather than approvals.
- Cleanup failures should preserve the primary error while appending cleanup diagnostics.
- Never launch detached jobs, poll session status, or depend on result files for normal delivery.

## Result semantics

Document review has structured CLI output and should be normalized before delivery. Code review has textual CLI output and should be returned without inventing unsupported approval JSON.

A tool result must clearly identify:

- target reviewed;
- approval, annotations, dismissal, or cancellation when known;
- feedback text;
- whether the agent should revise and automatically resubmit.

The agent instructions own looping decisions. The shared module reports facts and never edits repository files outside its isolated temporary range workspace.

## Error handling

- **Missing `plannotator`:** report the expected executable and installation action.
- **Invalid document path:** reject before process startup.
- **Unsupported diff target:** show accepted forms; do not fall back to current review.
- **Invalid base:** report the failed revision without executing it through a shell.
- **Ambiguous PR number:** list candidate remotes and require a full URL.
- **PR re-review cannot match the active checkout:** refuse to show stale remote content; request a matching checkout or pushed fixes.
- **No Git repository for local/range/number targets:** fail before opening Plannotator.
- **Process failure:** include bounded stderr and exit status.
- **Malformed document decision JSON:** fail closed and retain raw bounded output for diagnosis.
- **Cancellation:** kill descendants and clean temporary state.
- **Dismissal:** return dismissal as a deliberate human outcome, not an approval and not a process error.

## Testing strategy

Use test-driven development with fake `plannotator` and Git fixtures.

### Shared module tests

- closed target grammar accepts only documented forms;
- unsupported positionals never reach Plannotator;
- document paths cannot escape `cwd` and must be non-empty Markdown files;
- document approval, approval notes, annotations, and dismissal parse correctly;
- full URL forwarding preserves argument boundaries;
- bare-number resolution handles GitHub, GitLab, SSH/HTTPS remotes, subgroups, ambiguous remotes, and unknown enterprise hosts;
- `base..HEAD` and `base...HEAD` resolve safely;
- range materialization includes committed, staged, unstaged, untracked, deleted, renamed, and binary changes;
- a second materialization after simulated fixes contains the complete updated delta;
- foreground waits longer than ordinary Bash defaults without timing out;
- cancellation kills the child and cleans all temporary paths;
- process and cleanup errors remain actionable.

### Adapter tests

- Pi registers exactly the two expected model-callable tools and forwards `cwd`/signal/results;
- OpenCode exports exactly the two expected tools and forwards directory/signal/results;
- neither adapter invokes a shell tool, detaches work, polls, or injects a follow-up message;
- both use the same shared module interface.

### Deployment verification

- JavaScript syntax/tests pass;
- TypeScript adapters typecheck/load in their hosts;
- `./link.sh --dry-run` shows only intended links;
- a Pi smoke session sees both tools;
- an OpenCode smoke session sees both tools;
- manual document feedback resumes the same tool call;
- manual local, base-range, PR-number, and PR-URL reviews open the expected target;
- feedback causes automatic full re-review after changes;
- PR/MR re-review includes local fixes when the checkout matches and never pushes automatically or silently rereviews stale remote content.

## Rollout

1. Implement and test the shared module against fake processes and temporary Git repositories.
2. Add and load-test the Pi adapter.
3. Add and load-test the OpenCode adapter.
4. Add global offer/review-loop instructions.
5. Link with Stow and run host smoke tests.
6. Leave existing Plannotator commands and skills installed as manual fallbacks.

## Risks and mitigations

- **Semantic offer trigger is model-dependent:** keep the global wording short and explicit; do not claim hook-level enforcement.
- **Range materialization complexity:** constrain the interface to base-through-HEAD and test complete working-tree coverage exhaustively.
- **Ambiguous remotes:** fail rather than guess.
- **Long-running browser session:** own the child directly and omit timeouts.
- **Stale approval after edits:** every automatic resubmission rematerializes/rereads current content.
- **Host differences:** isolate them in two adapters; keep lifecycle behavior in the shared module.
- **OpenCode runtime churn:** test against the installed version and keep the adapter interface minimal.
- **User's installer work is in progress:** do not edit or commit those files as part of this feature.
