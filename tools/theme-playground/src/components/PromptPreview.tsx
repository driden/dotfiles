import { useMemo } from "react";
import { AnsiUp } from "ansi_up";

type Props = {
  ansi: string | null;
  highlight: { hex: string; role: "fg" | "bg" } | null;
};

const RGB_FG_RE = /(?:^|;)\s*color\s*:\s*rgb\(([^)]+)\)/;
const RGB_BG_RE = /(?:^|;)\s*background-color\s*:\s*rgb\(([^)]+)\)/;

function rgbToHex(triple: string): string {
  const [r, g, b] = triple.split(",").map(s => parseInt(s.trim(), 10));
  return "#" + [r, g, b].map(n => n.toString(16).padStart(2, "0")).join("").toUpperCase();
}

function annotateSpans(html: string): string {
  return html.replace(/<span style="([^"]*)">/g, (_match, style) => {
    const fg = RGB_FG_RE.exec(style);
    const bg = RGB_BG_RE.exec(style);
    const attrs: string[] = [];
    if (fg) attrs.push(`data-fg="${rgbToHex(fg[1])}"`);
    if (bg) attrs.push(`data-bg="${rgbToHex(bg[1])}"`);
    return `<span style="${style}" ${attrs.join(" ")}>`;
  });
}

export function PromptPreview({ ansi, highlight }: Props) {
  const html = useMemo(() => {
    if (!ansi) return "(no preview)";
    const ansi_up = new AnsiUp();
    ansi_up.use_classes = false;
    return annotateSpans(ansi_up.ansi_to_html(ansi));
  }, [ansi]);

  // Dynamic CSS rule: highlight spans whose role-matching channel uses this hex.
  const highlightCss = useMemo(() => {
    if (!highlight) return "";
    const attr = highlight.role === "fg" ? "data-fg" : "data-bg";
    return `.prompt-preview span[${attr}="${highlight.hex.toUpperCase()}"] {
      outline: 2px solid #fff;
      outline-offset: -1px;
      filter: brightness(1.15);
    }`;
  }, [highlight]);

  return (
    <>
      {highlightCss && <style>{highlightCss}</style>}
      <pre className="prompt-preview" dangerouslySetInnerHTML={{ __html: html }} />
    </>
  );
}
