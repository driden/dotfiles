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
const _require = createRequire(import.meta.url);
const _wasmPath = _require.resolve(
  "@tree-sitter-grammars/tree-sitter-toml/tree-sitter-toml.wasm",
);
const _lang = await Language.load(_wasmPath);
const _parser = new Parser();
_parser.setLanguage(_lang);

// ── helpers ──────────────────────────────────────────────────────────────────

// [contentStart, contentEnd] for a TOML string node — strips delimiters (1 or 3 chars).
function stringContent(node: TreeSitter.Node): [number, number] {
  const d = node.child(0)!.text.length; // 1 for " / ', 3 for """ / '''
  return [node.startIndex + d, node.endIndex - d];
}

function tableName(table: TreeSitter.Node): string {
  for (let i = 0; i < table.childCount; i++) {
    const c = table.child(i)!;
    if (c.type === "bare_key" || c.type === "dotted_key") return c.text;
  }
  return "format";
}

// Walk up to the nearest table ancestor; return its header key or "format".
function sectionOf(node: TreeSitter.Node): string {
  let cur: TreeSitter.Node | null = node.parent;
  while (cur) {
    if (cur.type === "table") return tableName(cur);
    cur = cur.parent;
  }
  return "format";
}

// ── stage 2: tokenize one style slice ────────────────────────────────────────

const TOKEN_RE = /(fg:|bg:)?([A-Za-z_][A-Za-z0-9_]*)/g;

function tokenizeSlice(
  source: string, sliceStart: number, sliceEnd: number,
  section: string, field: string, palette: Set<string>,
): ColorSlot[] {
  const text = source.slice(sliceStart, sliceEnd);
  const out: ColorSlot[] = [];
  let occ = 0;
  TOKEN_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = TOKEN_RE.exec(text)) !== null) {
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

const BRACKET_RE = /\]\(([^)]*)\)/g;

function bracketSlots(
  source: string, sliceStart: number, sliceEnd: number,
  section: string, field: string, palette: Set<string>,
): ColorSlot[] {
  const text = source.slice(sliceStart, sliceEnd);
  const out: ColorSlot[] = [];
  let occ = 0;
  BRACKET_RE.lastIndex = 0;
  let m: RegExpExecArray | null;
  while ((m = BRACKET_RE.exec(text)) !== null) {
    occ += 1;
    const innerStart = sliceStart + m.index + 2; // skip `](`
    out.push(...tokenizeSlice(source, innerStart, innerStart + m[1].length, section, `${field} (#${occ})`, palette));
  }
  return out;
}

// ── public API ───────────────────────────────────────────────────────────────

export function discoverSlots(text: string, palette: Set<string>, mode: SlotMode): ColorSlot[] {
  if (mode === "hex-literal") throw new Error("TODO(v2): hex-literal mode not implemented yet");

  const tree = _parser.parse(text)!;
  const styleOut: ColorSlot[] = [];
  const bracketOut: ColorSlot[] = [];

  // Walk pairs; accumulate style-field slots and bracket slots separately so
  // final order matches the original: all style-field tokens then all bracket tokens.
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
      return;
    }
    for (let i = 0; i < node.childCount; i++) visit(node.child(i)!);
  }

  visit(tree.rootNode);
  return [...styleOut, ...bracketOut];
}

export function paletteKeysFromStarshipToml(text: string): Set<string> {
  const tree = _parser.parse(text)!;
  const out = new Set<string>();
  for (let i = 0; i < tree.rootNode.childCount; i++) {
    const node = tree.rootNode.child(i)!;
    if (node.type !== "table") continue;
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
