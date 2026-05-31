import { useEffect, useState } from "react";
import type { ColorSlot } from "../lib/slot-discovery";
import { PalettePicker } from "./PalettePicker";

type Props = {
  slots: ColorSlot[];
  palette: Record<string, string>;
  activeModules: Set<string>;
  onEdit: (slotId: string, newKey: string) => void;
  onSlotDisappeared: () => void;
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

export function ColorSlotTable({ slots, palette, activeModules, onEdit, onSlotDisappeared }: Props) {
  const [openSlotId, setOpenSlotId] = useState<string | null>(null);

  useEffect(() => {
    if (openSlotId && !slots.find(s => s.id === openSlotId)) {
      setOpenSlotId(null);
      onSlotDisappeared();
    }
  }, [slots, openSlotId, onSlotDisappeared]);

  const groups = groupSlots(slots);
  const transitions: Group[] = [];
  const activeGroups: Group[] = [];
  const inactiveGroups: Group[] = [];
  for (const g of groups) {
    if (g.section === "format") transitions.push(g);
    else if (activeModules.has(g.section)) activeGroups.push(g);
    else inactiveGroups.push(g);
  }

  function renderCell(slot?: ColorSlot) {
    if (!slot) return <span className="empty-cell">—</span>;
    return (
      <span className="slot-cell">
        <span
          className="swatch inline clickable"
          style={{ background: palette[slot.key.toLowerCase()] ?? "#000" }}
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
        {renderGroup("prompt transitions", transitions, "t")}
        {renderGroup("active modules", activeGroups, "a")}
        {renderGroup("defined but unused", inactiveGroups, "i")}
      </tbody>
    </table>
  );
}
