import fs from "node:fs/promises";
import path from "node:path";
import os from "node:os";
import TOML from "@iarna/toml";
import { discoverSlots, paletteKeysFromStarshipToml, type ColorSlot } from "./src/lib/slot-discovery";

const REPO_ROOT = path.resolve(import.meta.dir, "../..");
const THEMES_DIR = path.join(REPO_ROOT, "themes");

type AppState = {
  app: "starship";
  fileRaw: string;
  colorSlots: ColorSlot[];
  preview: { kind: "ansi"; data: string } | null;
  error: string | null;
};

type ThemeState = {
  name: string;
  palette: Record<string, string>;   // from colors.toml
  apps: AppState[];
};

async function listThemes(): Promise<{ name: string; current: boolean }[]> {
  const entries = await fs.readdir(THEMES_DIR, { withFileTypes: true });
  const names = entries
    .filter(e => e.isDirectory() && e.name !== "templates")
    .map(e => e.name)
    .sort();

  let current = "";
  try {
    const namePath = path.join(os.homedir(), ".config/themes/current/name");
    current = (await fs.readFile(namePath, "utf8")).trim();
  } catch { /* not configured */ }

  return names.map(name => ({ name, current: name === current }));
}

async function readPalette(themeName: string): Promise<Record<string, string>> {
  const text = await fs.readFile(path.join(THEMES_DIR, themeName, "colors.toml"), "utf8");
  const parsed = TOML.parse(text) as { palette?: Record<string, string> };
  return parsed.palette ?? {};
}

async function renderStarship(configPath: string): Promise<{ ansi: string | null; error: string | null }> {
  const env = {
    PATH: process.env.PATH ?? "/usr/bin:/bin",
    HOME: process.env.HOME ?? os.homedir(),
    LANG: process.env.LANG ?? "en_US.UTF-8",
    TERM: "xterm-256color",
    STARSHIP_CONFIG: configPath,
    STARSHIP_SHELL: "zsh",
  };
  const proc = Bun.spawn(
    ["starship", "prompt",
     "--terminal-width=120",
     "--status=0",
     "--cmd-duration=1234",
     "--jobs=0"],
    {
      cwd: REPO_ROOT,
      env,
      stdout: "pipe",
      stderr: "pipe",
    },
  );
  const [stdout, stderr] = await Promise.all([
    new Response(proc.stdout).text(),
    new Response(proc.stderr).text(),
  ]);
  const exit = await proc.exited;
  if (exit !== 0) return { ansi: null, error: stderr.trim() || `starship exited ${exit}` };
  return { ansi: stdout, error: null };
}

async function buildAppState(themeName: string): Promise<AppState> {
  const configPath = path.join(THEMES_DIR, themeName, "starship.toml");
  const fileRaw = await fs.readFile(configPath, "utf8");
  const palette = paletteKeysFromStarshipToml(fileRaw);
  let colorSlots: ColorSlot[] = [];
  let stripError: string | null = null;
  try {
    colorSlots = discoverSlots(fileRaw, palette, "name-token");
  } catch (e: any) {
    stripError = e.message;
  }
  const { ansi, error } = await renderStarship(configPath);
  return {
    app: "starship",
    fileRaw,
    colorSlots,
    preview: ansi !== null ? { kind: "ansi", data: ansi } : null,
    error: error ?? stripError,
  };
}

async function buildThemeState(themeName: string): Promise<ThemeState> {
  return {
    name: themeName,
    palette: await readPalette(themeName),
    apps: [await buildAppState(themeName)],
  };
}

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
      const editMatch = p.match(/^\/api\/themes\/([\w-]+)\/([\w-]+)$/);
      if (req.method === "POST" && editMatch) {
        const [, themeName, app] = editMatch;
        if (app !== "starship") return json({ error: `app '${app}' not supported in v1` }, 404);
        const body = await req.json() as { slotId: string; newPaletteKey: string };
        const configPath = path.join(THEMES_DIR, themeName, "starship.toml");
        const current = await fs.readFile(configPath, "utf8");
        const palette = paletteKeysFromStarshipToml(current);
        if (!palette.has(body.newPaletteKey.toLowerCase())) {
          return json({ error: `key '${body.newPaletteKey}' not in [palettes.theme] — run \`theme build\`?` }, 400);
        }
        const slots = discoverSlots(current, palette, "name-token");
        const slot = slots.find(s => s.id === body.slotId);
        if (!slot) return json({ error: `slot '${body.slotId}' not found in current file (file may have changed)` }, 409);
        const next = current.slice(0, slot.start) + body.newPaletteKey + current.slice(slot.end);
        await fs.writeFile(configPath, next, "utf8");
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
