import { describe, expect, test } from "bun:test";
import { discoverSlots, paletteKeysFromStarshipToml } from "../src/lib/slot-discovery";

const PALETTE = new Set([
  "accent","cursor","foreground","background",
  "selection_foreground","selection_background",
  "color0","color1","color2","color3","color4","color5","color6","color7",
  "color8","color9","color10","color11","color12","color13","color14","color15",
]);

describe("discoverSlots (name-token mode)", () => {
  test("captures fg: and bg: tokens in a style field", () => {
    const text = `[os]\nstyle = "bg:color12 fg:foreground"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(2);
    expect(slots[0]).toMatchObject({ section: "os", field: "style", role: "bg", key: "color12" });
    expect(slots[1]).toMatchObject({ section: "os", field: "style", role: "fg", key: "foreground" });
  });

  test("captures bracketed [](fg:X bg:Y) constructs", () => {
    const text = `format = """\n[](fg:color12 bg:accent)\n"""\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots).toEqual([
      expect.objectContaining({ role: "fg", key: "color12" }),
      expect.objectContaining({ role: "bg", key: "accent" }),
    ]);
  });

  test("captures bare-color tokens as fg (starship default)", () => {
    const text = `format = """\n[](color12)\n"""\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ role: "fg", key: "color12" });
  });

  test("captures style_user and style_root (style_* prefix)", () => {
    const text = `[username]\nstyle_user = "bg:accent fg:background"\nstyle_root = "bg:accent fg:background"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(4);
    expect(slots.filter(s => s.field === "style_user").length).toBe(2);
    expect(slots.filter(s => s.field === "style_root").length).toBe(2);
  });

  test("captures *_style suffix variants", () => {
    const text = `cmd_duration_style = "fg:color3"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ field: "cmd_duration_style", role: "fg", key: "color3" });
  });

  test("skips hyphenated key names (regex token shape)", () => {
    const text = `pane-active-border-style = "fg:accent"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(0);
  });

  test("rejects literal hex (#RRGGBB) — not a palette ref", () => {
    const text = `format = '[[ $context ](fg:#83a598 bg:color8)]($style)'\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ role: "bg", key: "color8" });
  });

  test("rejects modifiers (bold, italic, dimmed, none, etc.)", () => {
    const text = `success_symbol = '[](bold fg:color2)'\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ role: "fg", key: "color2" });
  });

  test("rejects $style template-var inside ]($style)", () => {
    const text = `format = '[[ $branch ](fg:background bg:color9)]($style)'\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(2);
    expect(slots.map(s => s.key)).toEqual(["background", "color9"]);
  });

  test("lowercases keys before palette lookup", () => {
    const text = `style = "bg:Color5"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ key: "Color5" });
  });

  test("tracks section context across multiple blocks", () => {
    const text = [
      `[directory]`,
      `style = "fg:background bg:color5"`,
      ``,
      `[git_branch]`,
      `style = "bg:color6"`,
    ].join("\n");
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.map(s => s.section)).toEqual(["directory", "directory", "git_branch"]);
  });

  test("byte-offsets point at the start of the key name", () => {
    const text = `style = "bg:color12 fg:foreground"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    const slot = slots.find(s => s.key === "color12")!;
    expect(text.slice(slot.start, slot.end)).toBe("color12");
  });

  test("preserves write-back exactness via splice", () => {
    const text = `style = "bg:color12 fg:foreground"\n`;
    const slots = discoverSlots(text, PALETTE, "name-token");
    const slot = slots.find(s => s.key === "color12")!;
    const next = text.slice(0, slot.start) + "color3" + text.slice(slot.end);
    expect(next).toBe(`style = "bg:color3 fg:foreground"\n`);
  });

  test("strips comments outside triple-quoted strings (avoids fake bracket matches)", () => {
    const text = [
      `# this comment ]( contains a fake bracket match — fg:color5 )`,
      `style = "bg:accent"`,
    ].join("\n");
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ key: "accent" });
  });

  test("does NOT strip comments inside triple-quoted format strings", () => {
    const text = [
      `format = """`,
      `#$c\\`,
      `[](fg:color12)\\`,
      `"""`,
    ].join("\n");
    const slots = discoverSlots(text, PALETTE, "name-token");
    expect(slots.length).toBe(1);
    expect(slots[0]).toMatchObject({ key: "color12" });
  });

  test("hex-literal mode throws TODO(v2) (forward-compat stub)", () => {
    expect(() => discoverSlots("anything", PALETTE, "hex-literal"))
      .toThrow(/TODO\(v2\)/);
  });
});

import fs from "node:fs";
import path from "node:path";

describe("slot discovery — golden fixtures", () => {
  const repo = "/Users/driden/code/dotfiles";
  for (const theme of ["kanagawa", "bamboo"]) {
    test(`${theme} discovery matches committed fixture`, () => {
      const tomlPath = `${repo}/themes/${theme}/starship.toml`;
      const fxPath = path.join(__dirname, "fixtures", `${theme}-slots.json`);
      const text = fs.readFileSync(tomlPath, "utf8");
      const palette = paletteKeysFromStarshipToml(text);
      const slots = discoverSlots(text, palette, "name-token");
      const expected = JSON.parse(fs.readFileSync(fxPath, "utf8"));
      expect(slots).toEqual(expected);
    });
  }
});
