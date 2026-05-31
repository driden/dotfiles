import { useEffect, useState } from "react";
import type { ColorSlot } from "../lib/slot-discovery";
import { PalettePicker } from "./PalettePicker";

type Props = {
  slots: ColorSlot[];
  palette: Record<string, string>;
  onEdit: (slotId: string, newKey: string) => void;
  onSlotDisappeared: () => void;
};

export function ColorSlotTable({ slots, palette, onEdit, onSlotDisappeared }: Props) {
  const [openSlotId, setOpenSlotId] = useState<string | null>(null);

  // Race guard: if the slot list changes and the open picker's slot id
  // is no longer present, auto-close and signal the parent so it can toast.
  useEffect(() => {
    if (openSlotId && !slots.find(s => s.id === openSlotId)) {
      setOpenSlotId(null);
      onSlotDisappeared();
    }
  }, [slots, openSlotId, onSlotDisappeared]);

  return (
    <table className="slot-table">
      <thead>
        <tr><th>Section</th><th>Field</th><th>Role</th><th>Current</th><th></th></tr>
      </thead>
      <tbody>
        {slots.map(slot => (
          <tr key={slot.id}>
            <td>{slot.section}</td>
            <td>{slot.field}</td>
            <td>{slot.role}</td>
            <td>
              <span
                className="swatch"
                style={{
                  background: palette[slot.key.toLowerCase()] ?? "#000",
                  display: "inline-block",
                  verticalAlign: "middle",
                }}
              />
              {" "}{slot.key}
            </td>
            <td style={{ position: "relative" }}>
              <button onClick={() => setOpenSlotId(slot.id)}>pick</button>
              {openSlotId === slot.id && (
                <PalettePicker
                  palette={palette}
                  onPick={key => { onEdit(slot.id, key); setOpenSlotId(null); }}
                  onClose={() => setOpenSlotId(null)}
                />
              )}
            </td>
          </tr>
        ))}
      </tbody>
    </table>
  );
}
