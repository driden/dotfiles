import { useEffect, useState } from "react";
import { listThemes, getTheme, editSlot, type ThemeListing, type ThemeState, type AppState } from "./api";
import { PromptPreview } from "./components/PromptPreview";

export default function App() {
  const [themes, setThemes] = useState<ThemeListing[]>([]);
  const [activeName, setActiveName] = useState<string | null>(null);
  const [theme, setTheme] = useState<ThemeState | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [toast, setToast] = useState<string | null>(null);

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
        <AppSection key={app.app} theme={theme} app={app} onEdit={handleEdit} />
      ))}
      {toast && <div className="toast">{toast}</div>}
    </div>
  );
}

// Placeholders — real components arrive in Tasks 9-12.
function ThemeSelector(_: { themes: ThemeListing[]; active: string | null; onChange: (s: string) => void }) {
  return <div>ThemeSelector (TODO)</div>;
}
function PaletteStrip(_: { palette: Record<string, string> }) {
  return <div className="palette-strip">PaletteStrip (TODO)</div>;
}
function AppSection({ app }: { theme: ThemeState; app: AppState; onEdit: (id: string, k: string) => void }) {
  return (
    <section className="app-section">
      <h2>{app.app}</h2>
      <PromptPreview ansi={app.preview?.data ?? null} />
      <div>{app.colorSlots.length} slot(s) — table TODO</div>
    </section>
  );
}
