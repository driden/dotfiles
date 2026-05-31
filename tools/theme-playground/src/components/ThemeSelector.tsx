import type { ThemeListing } from "../api";

type Props = {
  themes: ThemeListing[];
  active: string | null;
  onChange: (name: string) => void;
};

export function ThemeSelector({ themes, active, onChange }: Props) {
  return (
    <label>
      Theme:{" "}
      <select value={active ?? ""} onChange={e => onChange(e.target.value)}>
        {themes.map(t => (
          <option key={t.name} value={t.name}>
            {t.name}{t.current ? " (current)" : ""}
          </option>
        ))}
      </select>
    </label>
  );
}
