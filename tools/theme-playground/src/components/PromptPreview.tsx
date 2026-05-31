import { useMemo } from "react";
import { AnsiUp } from "ansi_up";

type Props = { ansi: string | null };

export function PromptPreview({ ansi }: Props) {
  const html = useMemo(() => {
    if (!ansi) return "(no preview)";
    const ansi_up = new AnsiUp();
    ansi_up.use_classes = false;     // inline-style colors, no class lookup
    return ansi_up.ansi_to_html(ansi);
  }, [ansi]);
  return (
    <pre className="prompt-preview" dangerouslySetInnerHTML={{ __html: html }} />
  );
}
