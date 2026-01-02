import { useRef, useEffect } from "react";
import * as monaco from "monaco-editor";
import { registerPureScript } from "./purescript";
import { useTheme } from "../../contexts/ThemeContext";

/**
 * Detects macOS double-space-to-period substitutions and returns edits to revert them.
 * Returns null if no substitutions were detected.
 */
function detectDoubleSpaceSubstitutions(
  changes: readonly monaco.editor.IModelContentChange[],
  model: monaco.editor.ITextModel
): monaco.editor.IIdentifiedSingleEditOperation[] | null {
  const fixes: monaco.editor.IIdentifiedSingleEditOperation[] = [];

  for (const change of changes) {
    // macOS replaces 1 char (space) with ". " when double-space is typed
    if (change.text === ". " && change.rangeLength === 1) {
      const pos = model.getPositionAt(change.rangeOffset);
      fixes.push({
        range: new monaco.Range(
          pos.lineNumber,
          pos.column,
          pos.lineNumber,
          pos.column + 2
        ),
        text: "  ",
      });
    }
  }

  return fixes.length > 0 ? fixes : null;
}

interface Props {
  value: string;
  onChange: (value: string) => void;
}

export function MonacoEditor({ value, onChange }: Props) {
  const containerRef = useRef<HTMLDivElement>(null);
  const editorRef = useRef<monaco.editor.IStandaloneCodeEditor | null>(null);
  const onChangeRef = useRef(onChange);
  const isUpdatingRef = useRef(false);
  const { monacoTheme } = useTheme();

  // Keep onChange ref up to date to avoid stale closures
  useEffect(() => {
    onChangeRef.current = onChange;
  }, [onChange]);

  useEffect(() => {
    if (!containerRef.current) return;

    registerPureScript();

    const editor = monaco.editor.create(containerRef.current, {
      value,
      language: "purescript",
      theme: monacoTheme,
      automaticLayout: true,
      minimap: { enabled: false },
      folding: false,
      showFoldingControls: "never",
      stickyScroll: { enabled: false },
      fontSize: 16,
      fontFamily: "'Iosevka', ui-monospace, monospace",
      lineNumbers: "on",
      scrollBeyondLastLine: false,
      padding: { top: 16, bottom: 16 },
      renderLineHighlight: "line",
      cursorBlinking: "smooth",
      smoothScrolling: true,
    });

    editor.onDidChangeModelContent((e) => {
      if (isUpdatingRef.current) return;

      const model = editor.getModel();
      if (model) {
        const fixes = detectDoubleSpaceSubstitutions(e.changes, model);
        if (fixes) {
          isUpdatingRef.current = true;
          editor.executeEdits("double-space-fix", fixes);
          isUpdatingRef.current = false;
          onChangeRef.current(editor.getValue());
          return;
        }
      }

      onChangeRef.current(editor.getValue());
    });

    editorRef.current = editor;

    // Disable iOS/macOS double-space-to-period behavior
    const textarea = containerRef.current.querySelector("textarea");
    if (textarea) {
      textarea.setAttribute("autocorrect", "off");
      textarea.setAttribute("autocapitalize", "off");
    }

    return () => {
      editor.dispose();
    };
  }, []);

  // Sync external value changes
  useEffect(() => {
    const editor = editorRef.current;
    if (editor && editor.getValue() !== value) {
      isUpdatingRef.current = true;
      editor.setValue(value);
      isUpdatingRef.current = false;
    }
  }, [value]);

  // Update theme when it changes
  useEffect(() => {
    if (editorRef.current) {
      monaco.editor.setTheme(monacoTheme);
    }
  }, [monacoTheme]);

  return <div ref={containerRef} className="h-full w-full" />;
}
