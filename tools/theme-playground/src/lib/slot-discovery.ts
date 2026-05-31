// Slot discovery for starship.toml (name-token mode).
// Uses tree-sitter-toml for TOML structure; hand-rolled code only for
// the starship DSL (style tokenizer + [label](style) extractor).
// top-level await initialises the parser once at module load.

import * as TreeSitter from "web-tree-sitter";
import { createRequire } from "node:module";

export type SlotRole = "fg" | "bg";
export type SlotMode = "name-token" | "hex-literal";

export type ColorSlot = {
  id: string;       // stable: `${section}/${field}/${role}/${occ}@${start}`
  section: string;  // table name; root context -> "format"
  field: string;    // e.g. "style", "style_user", or "format (#N)" for bracketed
  role: SlotRole;
  key: string;      // captured token, original case preserved
  start: number;    // JS-string index of first char of key
  end: number;      // exclusive
};

// Lifted from starship's parse_style_string (src/config.rs:382-389).
const MODIFIERS = new Set([
  "underline", "bold", "italic", "dimmed", "inverted",
  "blink", "hidden", "strikethrough", "prev_fg", "prev_bg", "none",
]);

function isStyleField(name: string): boolean {
  return name === "style" || name.endsWith("_style") || name.startsWith("style_");
}

// ── tree-sitter init (top-level await) ───────────────────────────────────────

const { Parser, Language } = TreeSitter;
await Parser.init();
const require = createRequire(import.meta.url);
const wasmPath = require.resolve(
  "@tree-sitter-grammars/tree-sitter-toml/tree-sitter-toml.wasm",
);
const lang = await Language.load(wasmPath);
const parser = new Parser();
parser.setLanguage(lang);

// ── helpers ──────────────────────────────────────────────────────────────────

// [contentStart, contentEnd] for a TOML string node — strips delimiters (1 or 3 chars).
function stringContent(node: TreeSitter.Node): [number, number] {
  const d = node.child(0)!.text.length; // 1 for " / ', 3 for """ / '''
  return [node.startIndex + d, node.endIndex - d];
}

// Resolves the header key of a [table] or [[array-of-tables]] node.
// Handles bare (`foo`), dotted (`foo.bar`), and quoted (`"foo"`) keys.
function tableName(table: TreeSitter.Node): string {
  for (let i = 0; i < table.childCount; i++) {
    const c = table.child(i)!;
    if (c.type === "bare_key" || c.type === "dotted_key") return c.text;
    if (c.type === "quoted_key") return c.text.replace(/^["']|["']$/g, "");
  }
  return "format";
}

// Walk up to the nearest table ancestor; return its header key or "format".
function sectionOf(node: TreeSitter.Node): string {
  let cur: TreeSitter.Node | null = node.parent;
  while (cur) {
    if (cur.type === "table" || cur.type === "table_array_element") return tableName(cur);
    cur = cur.parent;
  }
  return "format";
}

// ── stage 2: tokenize one style slice ────────────────────────────────────────

function tokenizeSlice(
  source: string, sliceStart: number, sliceEnd: number,
  section: string, field: string, palette: Set<string>,
): ColorSlot[] {
  const re = /(fg:|bg:)?([A-Za-z_][A-Za-z0-9_]*)/g;
  const text = source.slice(sliceStart, sliceEnd);
  const out: ColorSlot[] = [];
  let occ = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    const prefix = m[1]; const name = m[2]; const lower = name.toLowerCase();
    if (MODIFIERS.has(lower) || !palette.has(lower)) continue;
    const role: SlotRole = prefix === "bg:" ? "bg" : "fg";
    const start = sliceStart + m.index + (prefix ? prefix.length : 0);
    occ += 1;
    out.push({ id: `${section}/${field}/${role}/${occ}@${start}`, section, field, role, key: name, start, end: start + name.length });
  }
  return out;
}

// ── stage 1b: [label](style) extractor ───────────────────────────────────────

// `occ` is per string value, not per (section, field) across the file. Works
// because each starship `format` is one string value, so per-value and
// per-field counters agree. If a future codebase splits formats across pairs,
// reconsider.
function bracketSlots(
  source: string, sliceStart: number, sliceEnd: number,
  section: string, field: string, palette: Set<string>,
): ColorSlot[] {
  const re = /\]\(([^)]*)\)/g;
  const text = source.slice(sliceStart, sliceEnd);
  const out: ColorSlot[] = [];
  let occ = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    occ += 1;
    const innerStart = sliceStart + m.index + 2; // skip `](`
    out.push(...tokenizeSlice(source, innerStart, innerStart + m[1].length, section, `${field} (#${occ})`, palette));
  }
  return out;
}

// ── public API ───────────────────────────────────────────────────────────────

export function discoverSlots(text: string, palette: Set<string>, mode: SlotMode): ColorSlot[] {
  if (mode === "hex-literal") throw new Error("TODO(v2): hex-literal mode not implemented yet");

  const tree = parser.parse(text)!;
  const styleOut: ColorSlot[] = [];
  const bracketOut: ColorSlot[] = [];

  // Walk every pair. Accumulate style-field slots and bracket slots separately
  // so the final order is: all style-field tokens, then all bracket tokens.
  // We don't early-return on pair so that inline_table values (which contain
  // nested pairs) are recursed into.
  function visit(node: TreeSitter.Node) {
    if (node.type === "pair") {
      const keyNode = node.child(0)!;
      if (keyNode.type === "bare_key") {
        const keyName = keyNode.text;
        const valNode = node.child(2);
        if (valNode?.type === "string") {
          const [cs, ce] = stringContent(valNode);
          const section = sectionOf(node);
          if (isStyleField(keyName)) styleOut.push(...tokenizeSlice(text, cs, ce, section, keyName, palette));
          bracketOut.push(...bracketSlots(text, cs, ce, section, keyName, palette));
        }
      }
    }
    for (let i = 0; i < node.childCount; i++) visit(node.child(i)!);
  }

  visit(tree.rootNode);
  return [...styleOut, ...bracketOut];
}

export function paletteKeysFromStarshipToml(text: string): Set<string> {
  const tree = parser.parse(text)!;
  const out = new Set<string>();
  for (let i = 0; i < tree.rootNode.childCount; i++) {
    const node = tree.rootNode.child(i)!;
    if (node.type !== "table") continue;
    // table layout: child(0) is "[", child(1) is the header key node.
    const headerKey = node.child(1)!;
    if (headerKey.type !== "dotted_key") continue;
    const parts = headerKey.text.split(".");
    if (parts[0] !== "palettes" || parts.length !== 2) continue;
    for (let j = 0; j < node.childCount; j++) {
      const child = node.child(j)!;
      if (child.type === "pair") {
        const k = child.child(0)!;
        if (k.type === "bare_key") out.add(k.text.toLowerCase());
      }
    }
  }
  return out;
}
