import type { ColorSlot } from "./lib/slot-discovery";

export type AppState = {
  app: "starship";
  fileRaw: string;
  colorSlots: ColorSlot[];
  preview: { kind: "ansi"; data: string } | null;
  error: string | null;
};

export type ThemeState = {
  name: string;
  palette: Record<string, string>;
  apps: AppState[];
};

export type ThemeListing = { name: string; current: boolean };

export async function listThemes(): Promise<ThemeListing[]> {
  const res = await fetch("/api/themes");
  if (!res.ok) throw new Error(`GET /api/themes failed: ${res.status}`);
  return res.json();
}

export async function getTheme(name: string): Promise<ThemeState> {
  const res = await fetch(`/api/themes/${name}`);
  if (!res.ok) throw new Error(`GET /api/themes/${name} failed: ${res.status}`);
  return res.json();
}

export async function editSlot(
  themeName: string,
  slotId: string,
  newPaletteKey: string,
): Promise<AppState> {
  const res = await fetch(`/api/themes/${themeName}/starship`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({ slotId, newPaletteKey }),
  });
  const body = await res.json();
  if (!res.ok) throw new Error(body.error ?? `edit failed (${res.status})`);
  return body as AppState;
}
