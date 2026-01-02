import { useEffect, useState } from "react";
import * as monaco from "monaco-editor";
import { useTheme } from "../contexts/ThemeContext";

interface Props {
  code: string;
}

export function HighlightedCode({ code }: Props) {
  const [html, setHtml] = useState<string>("");
  const { monacoTheme } = useTheme();

  useEffect(() => {
    let cancelled = false;

    monaco.editor
      .colorize(code, "purescript", { tabSize: 2 })
      .then((result) => {
        if (!cancelled) {
          setHtml(result);
        }
      })
      .catch(console.error);

    return () => {
      cancelled = true;
    };
  }, [code, monacoTheme]);

  if (!html) {
    return <pre className="font-mono text-base whitespace-pre-wrap text-fg-muted">{code}</pre>;
  }

  return (
    <pre
      className="font-mono text-base whitespace-pre-wrap"
      dangerouslySetInnerHTML={{ __html: html }}
    />
  );
}
