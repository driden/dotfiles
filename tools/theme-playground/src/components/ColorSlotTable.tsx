import { useEffect, useState } from "react";
import type { ColorSlot } from "../lib/slot-discovery";
import type { FormatToken } from "../App";
import { PalettePicker } from "./PalettePicker";

type Props = {
  slots: ColorSlot[];
  palette: Record<string, string>;
  formatTokens: FormatToken[];
  onEdit: (slotId: string, newKey: string) => void;
  onSlotDisappeared: () => void;
  onHoverSlot: (h: { hex: string; role: "fg" | "bg" } | null) => void;
};

type Group = {
  section: string;
  field: string;
  fg?: ColorSlot;
  bg?: ColorSlot;
};

// One Group per (section, field). Slots arrive interleaved by role; collapse
// them so we render one row per pair.
function groupSlots(slots: ColorSlot[]): Group[] {
  const map = new Map<string, Group>();
  const order: string[] = [];
  for (const s of slots) {
    const k = `${s.section}/${s.field}`;
    if (!map.has(k)) {
      map.set(k, { section: s.section, field: s.field });
      order.push(k);
    }
    const g = map.get(k)!;
    if (s.role === "fg") g.fg = s;
    else g.bg = s;
  }
  return order.map(k => map.get(k)!);
}

// Order `groups` to match the visual order of the rendered prompt by walking
// `formatTokens`: each transition pulls the next format-section group; each
// module reference pulls all that section's groups. Anything left over is
// "defined but unused".
function orderByPrompt(groups: Group[], formatTokens: FormatToken[]): { active: Group[]; inactive: Group[] } {
  const bySection = new Map<string, Group[]>();
  for (const g of groups) {
    if (!bySection.has(g.section)) bySection.set(g.section, []);
    bySection.get(g.section)!.push(g);
  }
  const formatGroups = bySection.get("format") ?? [];
  const active: Group[] = [];
  const seen = new Set<string>();
  const keyOf = (g: Group) => `${g.section}/${g.field}`;
  let formatIdx = 0;

  for (const tok of formatTokens) {
    if (tok.type === "transition") {
      if (formatIdx < formatGroups.length) {
        const g = formatGroups[formatIdx++];
        active.push(g);
        seen.add(keyOf(g));
      }
    } else {
      for (const g of (bySection.get(tok.name) ?? [])) {
        if (!seen.has(keyOf(g))) {
          active.push(g);
          seen.add(keyOf(g));
        }
      }
    }
  }
  while (formatIdx < formatGroups.length) {
    const g = formatGroups[formatIdx++];
    active.push(g);
    seen.add(keyOf(g));
  }

  const inactive = groups.filter(g => !seen.has(keyOf(g)));
  return { active, inactive };
}

export function ColorSlotTable({ slots, palette, formatTokens, onEdit, onSlotDisappeared, onHoverSlot }: Props) {
  const [openSlotId, setOpenSlotId] = useState<string | null>(null);

  useEffect(() => {
    if (openSlotId && !slots.find(s => s.id === openSlotId)) {
      setOpenSlotId(null);
      onSlotDisappeared();
    }
  }, [slots, openSlotId, onSlotDisappeared]);

  const groups = groupSlots(slots);
  const { active: activeGroups, inactive: inactiveGroups } = orderByPrompt(groups, formatTokens);

  function renderCell(slot?: ColorSlot) {
    if (!slot) return <span className="empty-cell">—</span>;
    const hex = (palette[slot.key.toLowerCase()] ?? "#000").toUpperCase();
    return (
      <span
        className="slot-cell"
        onMouseEnter={() => onHoverSlot({ hex, role: slot.role })}
        onMouseLeave={() => onHoverSlot(null)}
      >
        <span
          className="swatch inline clickable"
          style={{ background: hex }}
          onClick={() => setOpenSlotId(slot.id)}
          title="click to pick a new color"
        />
        <span className="slot-key">{slot.key}</span>
        {openSlotId === slot.id && (
          <PalettePicker
            palette={palette}
            onPick={k => { onEdit(slot.id, k); setOpenSlotId(null); }}
            onClose={() => setOpenSlotId(null)}
          />
        )}
      </span>
    );
  }

  function renderRow(g: Group, key: string) {
    return (
      <tr key={key}>
        <td>{g.section}</td>
        <td>{g.field}</td>
        <td>{renderCell(g.bg)}</td>
        <td>{renderCell(g.fg)}</td>
      </tr>
    );
  }

  function renderGroup(label: string, items: Group[], prefix: string) {
    if (items.length === 0) return null;
    return (
      <>
        <tr className="group-header"><td colSpan={4}>{label}</td></tr>
        {items.map((g, i) => renderRow(g, `${prefix}${i}`))}
      </>
    );
  }

  return (
    <table className="slot-table">
      <thead>
        <tr><th>Section</th><th>Field</th><th>BG</th><th>FG</th></tr>
      </thead>
      <tbody>
        {renderGroup("in your prompt", activeGroups, "a")}
        {renderGroup("defined but unused", inactiveGroups, "i")}
      </tbody>
    </table>
  );
}
