import fs from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import TOML from "@iarna/toml";
import { discoverSlots, paletteKeysFromStarshipToml, type ColorSlot } from "./src/lib/slot-discovery";

const REPO_ROOT = path.resolve(import.meta.dir, "../..");
const THEMES_DIR = path.join(REPO_ROOT, "themes");
const DRAFTS_DIR = path.join(import.meta.dir, ".drafts");

type AppState = {
  app: "starship";
  fileRaw: string;
  colorSlots: ColorSlot[];
  preview: { kind: "ansi"; data: string } | null;
  error: string | null;
  dirty: boolean;
  canUndo: boolean;
};

type ThemeState = {
  name: string;
  palette: Record<string, string>;
  apps: AppState[];
};

// ── path / draft helpers ─────────────────────────────────────────────────────

function originalPath(theme: string, app: string): string {
  return path.join(THEMES_DIR, theme, `${app}.toml`);
}
function draftPath(theme: string, app: string): string {
  return path.join(DRAFTS_DIR, `${theme}-${app}.toml`);
}

// First touch: copy original → draft. Idempotent on subsequent calls.
async function ensureDraft(theme: string, app: string): Promise<string> {
  await fs.mkdir(DRAFTS_DIR, { recursive: true });
  const draft = draftPath(theme, app);
  if (!(await Bun.file(draft).exists())) {
    const original = await fs.readFile(originalPath(theme, app), "utf8");
    await fs.writeFile(draft, original);
  }
  return draft;
}

async function isDirty(theme: string, app: string): Promise<boolean> {
  const d = await fs.readFile(draftPath(theme, app), "utf8");
  const o = await fs.readFile(originalPath(theme, app), "utf8");
  return d !== o;
}

// ── undo stack (in-memory, per theme+app) ────────────────────────────────────

const HISTORY_LIMIT = 50;
const histories = new Map<string, string[]>();
const histKey = (t: string, a: string) => `${t}/${a}`;

function pushHistory(theme: string, app: string, snapshot: string) {
  const k = histKey(theme, app);
  const stack = histories.get(k) ?? [];
  stack.push(snapshot);
  if (stack.length > HISTORY_LIMIT) stack.shift();
  histories.set(k, stack);
}
function popHistory(theme: string, app: string): string | null {
  return histories.get(histKey(theme, app))?.pop() ?? null;
}
function canUndo(theme: string, app: string): boolean {
  return (histories.get(histKey(theme, app)) ?? []).length > 0;
}
function clearHistory(theme: string, app: string) {
  histories.delete(histKey(theme, app));
}

// ── theme listing / palette ──────────────────────────────────────────────────

async function listThemes(): Promise<{ name: string; current: boolean }[]> {
  const entries = await fs.readdir(THEMES_DIR, { withFileTypes: true });
  const names = entries
    .filter(e => e.isDirectory() && e.name !== "templates")
    .map(e => e.name)
    .sort();

  let current = "";
  const namePath = path.join(os.homedir(), ".config/themes/current/name");
  const nameFile = Bun.file(namePath);
  if (await nameFile.exists()) current = (await nameFile.text()).trim();

  return names.map(name => ({ name, current: name === current }));
}

async function readPalette(themeName: string): Promise<Record<string, string>> {
  const text = await fs.readFile(path.join(THEMES_DIR, themeName, "colors.toml"), "utf8");
  const parsed = TOML.parse(text) as { palette?: Record<string, string> };
  return parsed.palette ?? {};
}

// ── starship subprocess ──────────────────────────────────────────────────────

async function renderStarship(configPath: string): Promise<{ ansi: string | null; error: string | null }> {
  const env = {
    PATH: process.env.PATH ?? "/usr/bin:/bin",
    HOME: process.env.HOME ?? os.homedir(),
    LANG: process.env.LANG ?? "en_US.UTF-8",
    TERM: "xterm-256color",
    STARSHIP_CONFIG: configPath,
    // Intentionally no STARSHIP_SHELL — setting it to zsh makes starship
    // wrap ANSI escapes in `%{...%}` markers for zsh's prompt-length counter,
    // which real zsh strips but our HTML preview shows literally.
  };
  const proc = Bun.spawn(
    ["starship", "prompt",
     "--terminal-width=120",
     "--status=0",
     "--cmd-duration=1234",
     "--jobs=0"],
    { cwd: REPO_ROOT, env, stdout: "pipe", stderr: "pipe" },
  );
  const [stdout, stderr] = await Promise.all([
    new Response(proc.stdout).text(),
    new Response(proc.stderr).text(),
  ]);
  const exit = await proc.exited;
  if (exit !== 0) return { ansi: null, error: stderr.trim() || `starship exited ${exit}` };
  return { ansi: stdout, error: null };
}

// ── theme state ──────────────────────────────────────────────────────────────

async function buildAppState(themeName: string): Promise<AppState> {
  const draft = await ensureDraft(themeName, "starship");
  const fileRaw = await fs.readFile(draft, "utf8");
  const palette = paletteKeysFromStarshipToml(fileRaw);
  let colorSlots: ColorSlot[] = [];
  let stripError: string | null = null;
  try {
    colorSlots = discoverSlots(fileRaw, palette, "name-token");
  } catch (e: any) {
    stripError = e.message;
  }
  const { ansi, error } = await renderStarship(draft);
  return {
    app: "starship",
    fileRaw,
    colorSlots,
    preview: ansi !== null ? { kind: "ansi", data: ansi } : null,
    error: error ?? stripError,
    dirty: await isDirty(themeName, "starship"),
    canUndo: canUndo(themeName, "starship"),
  };
}

async function buildThemeState(themeName: string): Promise<ThemeState> {
  return {
    name: themeName,
    palette: await readPalette(themeName),
    apps: [await buildAppState(themeName)],
  };
}

// ── http ─────────────────────────────────────────────────────────────────────

function json(value: unknown, status = 200): Response {
  return new Response(JSON.stringify(value), {
    status,
    headers: { "content-type": "application/json" },
  });
}

const server = Bun.serve({
  port: 5174,
  async fetch(req) {
    const url = new URL(req.url);
    const p = url.pathname;
    try {
      if (req.method === "GET" && p === "/api/themes") {
        return json(await listThemes());
      }
      const themeMatch = p.match(/^\/api\/themes\/([\w-]+)$/);
      if (req.method === "GET" && themeMatch) {
        return json(await buildThemeState(themeMatch[1]));
      }

      // POST /api/themes/:name/:app/(undo|save|discard) — draft actions
      const actionMatch = p.match(/^\/api\/themes\/([\w-]+)\/([\w-]+)\/(undo|save|discard)$/);
      if (req.method === "POST" && actionMatch) {
        const [, themeName, app, action] = actionMatch;
        if (app !== "starship") return json({ error: `app '${app}' not supported in v1` }, 404);
        const draft = draftPath(themeName, app);
        const original = originalPath(themeName, app);

        if (action === "undo") {
          const prev = popHistory(themeName, app);
          if (prev === null) return json({ error: "nothing to undo" }, 400);
          await fs.writeFile(draft, prev, "utf8");
        } else if (action === "save") {
          await ensureDraft(themeName, app);
          const draftText = await fs.readFile(draft, "utf8");
          await fs.writeFile(original, draftText, "utf8");
        } else if (action === "discard") {
          const originalText = await fs.readFile(original, "utf8");
          await fs.writeFile(draft, originalText, "utf8");
          clearHistory(themeName, app);
        }
        return json(await buildAppState(themeName));
      }

      // POST /api/themes/:name/:app — slot edit (writes to draft)
      const editMatch = p.match(/^\/api\/themes\/([\w-]+)\/([\w-]+)$/);
      if (req.method === "POST" && editMatch) {
        const [, themeName, app] = editMatch;
        if (app !== "starship") return json({ error: `app '${app}' not supported in v1` }, 404);
        const body = await req.json() as { slotId: string; newPaletteKey: string };
        const draft = await ensureDraft(themeName, app);
        const current = await fs.readFile(draft, "utf8");
        const palette = paletteKeysFromStarshipToml(current);
        if (!palette.has(body.newPaletteKey.toLowerCase())) {
          return json({ error: `key '${body.newPaletteKey}' not in [palettes.theme] — run \`theme build\`?` }, 400);
        }
        const slots = discoverSlots(current, palette, "name-token");
        const slot = slots.find(s => s.id === body.slotId);
        if (!slot) return json({ error: `slot '${body.slotId}' not found in current file (file may have changed)` }, 409);

        pushHistory(themeName, app, current);
        const next = current.slice(0, slot.start) + body.newPaletteKey + current.slice(slot.end);
        await fs.writeFile(draft, next, "utf8");
        return json(await buildAppState(themeName));
      }

      return new Response("not found", { status: 404 });
    } catch (e: any) {
      console.error(e);
      return json({ error: e.message ?? String(e) }, 500);
    }
  },
});

console.log(`theme-playground server listening on http://localhost:${server.port}`);
