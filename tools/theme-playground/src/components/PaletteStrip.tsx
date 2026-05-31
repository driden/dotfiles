type Props = { palette: Record<string, string> };

const STABLE_ORDER = [
  "accent", "cursor", "foreground", "background",
  "selection_foreground", "selection_background",
  "color0", "color1", "color2", "color3", "color4", "color5", "color6", "color7",
  "color8", "color9", "color10", "color11", "color12", "color13", "color14", "color15",
];

export function PaletteStrip({ palette }: Props) {
  const present = STABLE_ORDER.filter(k => palette[k]);
  return (
    <div className="palette-strip" role="list">
      {present.map(key => (
        <div
          key={key}
          role="listitem"
          className="swatch"
          style={{ background: palette[key] }}
          title={`${key} — ${palette[key]}`}
        />
      ))}
    </div>
  );
}
