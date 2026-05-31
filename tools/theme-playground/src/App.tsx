import { useEffect, useState } from "react";
import { listThemes, getTheme, editSlot, type ThemeListing, type ThemeState, type AppState } from "./api";
import { PromptPreview } from "./components/PromptPreview";
import { PaletteStrip } from "./components/PaletteStrip";
import { ColorSlotTable } from "./components/ColorSlotTable";
import { ThemeSelector } from "./components/ThemeSelector";

// Modules `$foo` referenced anywhere in the top-level `format` string —
// the "actually visible in your prompt" set. Used to put untouched language
// modules (nodejs, rust, etc.) below the fold.
function activeModules(fileRaw: string): Set<string> {
  const active = new Set<string>(["format"]);
  const m = fileRaw.match(/^format\s*=\s*(?:"""([\s\S]*?)"""|"([^"]*)"|'([^']*)')/m);
  const content = m ? (m[1] ?? m[2] ?? m[3] ?? "") : "";
  for (const r of content.matchAll(/\$\{?([A-Za-z_]\w*)/g)) active.add(r[1]);
  return active;
}

type HoverSlot = { hex: string; role: "fg" | "bg" } | null;

export default function App() {
  const [themes, setThemes] = useState<ThemeListing[]>([]);
  const [activeName, setActiveName] = useState<string | null>(null);
  const [theme, setTheme] = useState<ThemeState | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [toast, setToast] = useState<string | null>(null);
  const [hover, setHover] = useState<HoverSlot>(null);

  useEffect(() => {
    listThemes().then(list => {
      setThemes(list);
      const initial = list.find(t => t.current) ?? list[0];
      if (initial) setActiveName(initial.name);
    }).catch(e => setError(e.message));
  }, []);

  useEffect(() => {
    if (!activeName) return;
    setError(null);
    getTheme(activeName).then(setTheme).catch(e => setError(e.message));
  }, [activeName]);

  async function handleEdit(slotId: string, newKey: string) {
    if (!theme) return;
    try {
      const updated = await editSlot(theme.name, slotId, newKey);
      setTheme(prev => prev ? { ...prev, apps: [updated] } : prev);
    } catch (e: any) {
      setError(e.message);
    }
  }

  async function handleReload() {
    if (!activeName) return;
    setError(null);
    try {
      const t = await getTheme(activeName);
      setTheme(t);
      setToast("reloaded");
      setTimeout(() => setToast(null), 1500);
    } catch (e: any) {
      setError(e.message);
    }
  }

  return (
    <div>
      <header className="app-header">
        <ThemeSelector themes={themes} active={activeName} onChange={setActiveName} />
        <button onClick={handleReload}>reload</button>
      </header>
      {error && <div className="error-banner">{error}</div>}
      {theme && <PaletteStrip palette={theme.palette} />}
      {theme?.apps.map(app => (
        <AppSection
          key={app.app}
          theme={theme}
          app={app}
          hover={hover}
          onEdit={handleEdit}
          onHoverSlot={setHover}
          onSlotDisappeared={() => {
            setToast("slot moved — pick again");
            setTimeout(() => setToast(null), 1800);
          }}
        />
      ))}
      {toast && <div className="toast">{toast}</div>}
    </div>
  );
}

function AppSection({ theme, app, hover, onEdit, onSlotDisappeared, onHoverSlot }: {
  theme: ThemeState;
  app: AppState;
  hover: HoverSlot;
  onEdit: (id: string, k: string) => void;
  onSlotDisappeared: () => void;
  onHoverSlot: (h: HoverSlot) => void;
}) {
  return (
    <section className="app-section">
      <h2>{app.app}</h2>
      <PromptPreview ansi={app.preview?.data ?? null} highlight={hover} />
      <ColorSlotTable
        slots={app.colorSlots}
        palette={theme.palette}
        activeModules={activeModules(app.fileRaw)}
        onEdit={onEdit}
        onSlotDisappeared={onSlotDisappeared}
        onHoverSlot={onHoverSlot}
      />
    </section>
  );
}
