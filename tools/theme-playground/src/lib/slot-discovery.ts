// Pure slot discovery for starship.toml (name-token mode).
// Forward-compat stub for tmux/fzf (hex-literal mode).
//
// See docs/superpowers/specs/2026-05-31-theme-playground-design.md
// for the algorithm. Two regexes for style-slice extraction, one
// for tokenization, plus a triple-quote-aware comment pre-strip.

export type SlotRole = "fg" | "bg";
export type SlotMode = "name-token" | "hex-literal";

export type ColorSlot = {
  id: string;          // stable: `${section}/${field}/${role}/${occ}@${start}`
  section: string;    // "[directory]" -> "directory"; root context -> "format"
  field: string;       // "style", "style_user", "format", "success_symbol", …
  role: SlotRole;
  key: string;        // captured token, original case preserved
  start: number;      // byte offset of first char of `key`
  end: number;        // exclusive
};

// Lifted verbatim from starship's parse_style_string
// (src/config.rs:382-389 — see spec for the citation).
const MODIFIERS = new Set([
  "underline", "bold", "italic", "dimmed", "inverted",
  "blink", "hidden", "strikethrough", "prev_fg", "prev_bg", "none",
]);

// Style-bearing single-line field: name is `style`, ends with `_style`,
// OR starts with `style_`. The last case picks up `style_user`/`style_root`
// in the [username] block. The middle case picks up things like
// `window-status-style` style names used by other apps' templates — but
// dot-style starship key names use underscores (`pane_border_style` etc).
function isStyleField(name: string): boolean {
  return name === "style" || name.endsWith("_style") || name.startsWith("style_");
}

// Strip `# ...` to end-of-line, but only outside quoted regions.
// A small state machine; tracks:
//   - triple-quoted strings `"""..."""` and `'''...'''` (multi-line TOML)
//   - single-line strings `"..."` and `'...'`
// Inside any quoted region, `#` is not a comment character.
// Triple-quote check precedes single-quote check so `"""` is not
// misread as an empty `""` followed by a lone `"`.
function stripCommentsOutsideTripleQuotes(text: string): string {
  let out = "";
  let i = 0;
  let inTriple: '"""' | "'''" | null = null;
  let inSingle: '"' | "'" | null = null;
  while (i < text.length) {
    // Inside a triple-quoted region — scan until the matching close.
    if (inTriple) {
      if (text.startsWith(inTriple, i)) {
        out += inTriple;
        i += 3;
        inTriple = null;
      } else {
        out += text[i++];
      }
      continue;
    }
    // Inside a single-line quoted string — scan until close quote or newline.
    if (inSingle) {
      if (text[i] === "\n") {
        // Unterminated single-line string — treat newline as end.
        out += text[i++];
        inSingle = null;
      } else if (text[i] === "\\") {
        // Escape sequence — pass through both chars verbatim.
        out += text[i++];
        if (i < text.length) out += text[i++];
      } else if (text[i] === inSingle) {
        out += text[i++];
        inSingle = null;
      } else {
        out += text[i++];
      }
      continue;
    }
    // Outside any string: check for triple-quote openers first.
    if (text.startsWith('"""', i)) { out += '"""'; i += 3; inTriple = '"""'; continue; }
    if (text.startsWith("'''", i)) { out += "'''"; i += 3; inTriple = "'''"; continue; }
    // Single-line string openers.
    if (text[i] === '"') { out += text[i++]; inSingle = '"'; continue; }
    if (text[i] === "'") { out += text[i++]; inSingle = "'"; continue; }
    if (text[i] === "#") {
      // Replace `# ... \n` with same-length spaces so byte offsets are preserved.
      const nl = text.indexOf("\n", i);
      const endComment = nl === -1 ? text.length : nl;
      out += " ".repeat(endComment - i);
      i = endComment;
      continue;
    }
    out += text[i++];
  }
  return out;
}

function findSections(text: string): { name: string; start: number }[] {
  const re = /^\s*\[([\w.]+)\]\s*$/gm;
  const sections: { name: string; start: number }[] = [];
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    sections.push({ name: m[1], start: m.index });
  }
  return sections;
}

function sectionFor(sections: { name: string; start: number }[], offset: number): string {
  let cur = "format"; // root-context label, per spec
  for (const s of sections) {
    if (s.start > offset) break;
    cur = s.name;
  }
  return cur;
}

// Walk backward from `from` to find the most recent `^([A-Za-z_]\w*)\s*=` line.
// Used to label bracket-style matches with the containing field name (e.g.
// "format", "success_symbol").
function nearestFieldName(text: string, from: number): string | null {
  const slice = text.slice(0, from);
  const re = /(^|\n)\s*([A-Za-z_][A-Za-z0-9_]*)\s*=/g;
  let last: string | null = null;
  let m: RegExpExecArray | null;
  while ((m = re.exec(slice)) !== null) last = m[2];
  return last;
}

type Slice = { sliceStart: number; sliceEnd: number; section: string; field: string };

// Stage 1a: single-line `name = "..."` or `name = '...'` whose name is style-bearing.
function findStyleFieldSlices(text: string, sections: { name: string; start: number }[]): Slice[] {
  const re = /^([ \t]*)([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(["'])((?:(?!\3).)*)\3/gm;
  const out: Slice[] = [];
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    if (!isStyleField(m[2])) continue;
    const sliceStart = m.index + m[0].length - 1 - m[4].length;
    out.push({
      sliceStart,
      sliceEnd: sliceStart + m[4].length,
      section: sectionFor(sections, m.index),
      field: m[2],
    });
  }
  return out;
}

// Stage 1b: every `](...)` anywhere. Field label backward-scanned.
function findBracketStyleSlices(text: string, sections: { name: string; start: number }[]): Slice[] {
  const re = /\]\(([^)]*)\)/g;
  const out: Slice[] = [];
  let m: RegExpExecArray | null;
  const perFieldOcc = new Map<string, number>();
  while ((m = re.exec(text)) !== null) {
    const sliceStart = m.index + 2;
    const sliceEnd = sliceStart + m[1].length;
    const section = sectionFor(sections, m.index);
    const field = nearestFieldName(text, m.index) ?? "format";
    const occKey = `${section}/${field}`;
    const occ = (perFieldOcc.get(occKey) ?? 0) + 1;
    perFieldOcc.set(occKey, occ);
    out.push({
      sliceStart, sliceEnd, section,
      field: `${field} (#${occ})`,
    });
  }
  return out;
}

// Stage 2: tokenize one slice.
function tokenizeSlice(
  source: string,
  slice: Slice,
  palette: Set<string>,
): ColorSlot[] {
  const re = /(fg:|bg:)?([A-Za-z_][A-Za-z0-9_]*)/g;
  const text = source.slice(slice.sliceStart, slice.sliceEnd);
  const out: ColorSlot[] = [];
  let occ = 0;
  let m: RegExpExecArray | null;
  while ((m = re.exec(text)) !== null) {
    const prefix = m[1];
    const name = m[2];
    const nameOffsetInSlice = m.index + (prefix ? prefix.length : 0);
    const lower = name.toLowerCase();
    if (MODIFIERS.has(lower)) continue;
    if (!palette.has(lower)) continue;
    const role: SlotRole = prefix === "bg:" ? "bg" : "fg";
    const start = slice.sliceStart + nameOffsetInSlice;
    occ += 1;
    out.push({
      id: `${slice.section}/${slice.field}/${role}/${occ}@${start}`,
      section: slice.section,
      field: slice.field,
      role,
      key: name,
      start,
      end: start + name.length,
    });
  }
  return out;
}

export function discoverSlots(
  text: string,
  palette: Set<string>,
  mode: SlotMode,
): ColorSlot[] {
  if (mode === "hex-literal") {
    throw new Error("TODO(v2): hex-literal mode not implemented yet");
  }
  // name-token mode.
  // Triple-quote-aware comment strip (preserves byte offsets via space-padding).
  const stripped = stripCommentsOutsideTripleQuotes(text);
  const sections = findSections(stripped);

  // Pre-flight: refuse multi-line style fields.
  const multiline = /^\s*([A-Za-z_]\w*)\s*=\s*"""/m;
  const mm = multiline.exec(stripped);
  if (mm && isStyleField(mm[1])) {
    throw new Error(`unsupported: multi-line style field '${mm[1]}' near offset ${mm.index}`);
  }

  const slices = [
    ...findStyleFieldSlices(stripped, sections),
    ...findBracketStyleSlices(stripped, sections),
  ];
  // Tokenize each slice against the *stripped* text but emit offsets that
  // are valid in the *original* text — they're identical, because the strip
  // preserved length.
  return slices.flatMap(s => tokenizeSlice(text, s, palette));
}

// Helper used by the backend: parse the [palettes.theme] block out of a
// starship.toml and return its keys (lowercased). Single regex pass, no
// dependence on @iarna/toml — keeps this lib pure.
export function paletteKeysFromStarshipToml(text: string): Set<string> {
  const out = new Set<string>();
  const re = /^\s*\[palettes\.([\w]+)\]([\s\S]*?)(?=^\s*\[|$)/gm;
  const m = re.exec(text);
  if (!m) return out;
  const body = m[2];
  const lineRe = /^\s*([A-Za-z_]\w*)\s*=/gm;
  let lm: RegExpExecArray | null;
  while ((lm = lineRe.exec(body)) !== null) {
    out.add(lm[1].toLowerCase());
  }
  return out;
}
