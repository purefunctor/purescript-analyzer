import { useState, useEffect, useCallback } from "react";
import { useDocsLib } from "./hooks/useDocsLib";
import { useDebounce } from "./hooks/useDebounce";
import { MonacoEditor } from "./components/Editor/MonacoEditor";
import { Tabs } from "./components/Tabs";
import { CstPanel } from "./components/CstPanel";
import { TypeCheckerPanel } from "./components/TypeCheckerPanel";
import { PerformanceBar } from "./components/PerformanceBar";
import { ThemeSwitcher } from "./components/ThemeSwitcher";
import type { ParseResult, CheckResult, Mode, Timing } from "./lib/types";

const DEFAULT_SOURCE = `module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

deriveUnion :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
deriveUnion = Proxy

deriveUnionLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
deriveUnionLeft = Proxy

solveUnion = { deriveUnion, deriveUnionLeft }
`;

export default function App() {
  const docsLib = useDocsLib();
  const [source, setSource] = useState(DEFAULT_SOURCE);
  const [mode, setMode] = useState<Mode>("cst");
  const [parseResult, setParseResult] = useState<ParseResult | null>(null);
  const [checkResult, setCheckResult] = useState<CheckResult | null>(null);
  const [timing, setTiming] = useState<Timing | null>(null);

  const debouncedSource = useDebounce(source, 150);

  const runAnalysis = useCallback(async () => {
    if (docsLib.status !== "ready") return;

    try {
      // Always parse for CST mode
      const parsed = await docsLib.lib.parse(debouncedSource);
      setParseResult(parsed);
      setTiming({
        lex: parsed.lex,
        layout: parsed.layout,
        parse: parsed.parse,
      });

      // Run type checker if in that mode
      if (mode === "typechecker") {
        const checked = await docsLib.lib.check(debouncedSource);
        setCheckResult(checked);
        setTiming({
          lex: checked.timing.lex,
          layout: checked.timing.layout,
          parse: checked.timing.parse,
          stabilize: checked.timing.stabilize,
          index: checked.timing.index,
          resolve: checked.timing.resolve,
          lower: checked.timing.lower,
          check: checked.timing.check,
        });
      }
    } catch (err) {
      console.error("Analysis error:", err);
    }
  }, [docsLib, debouncedSource, mode]);

  useEffect(() => {
    runAnalysis();
  }, [runAnalysis]);

  if (docsLib.status === "loading") {
    return (
      <div className="flex h-screen items-center justify-center bg-bg text-fg">
        <div className="text-lg">Loading WASM module...</div>
      </div>
    );
  }

  if (docsLib.status === "error") {
    return (
      <div className="flex h-screen items-center justify-center bg-bg text-fg">
        <div className="text-lg text-red-400">Failed to load: {docsLib.error.message}</div>
      </div>
    );
  }

  return (
    <div className="flex h-screen flex-col bg-bg text-fg">
      <header className="flex items-center justify-between border-b border-bg-lighter px-5 py-3.5">
        <h1 className="font-sans text-lg font-semibold tracking-tight">PureScript Analyzer</h1>
        <div className="flex items-center gap-4">
          {timing && <PerformanceBar timing={timing} />}
          <ThemeSwitcher />
        </div>
      </header>

      <main className="grid flex-1 grid-cols-2 gap-0 overflow-hidden">
        <div className="h-full overflow-hidden border-r border-bg-lighter">
          <MonacoEditor value={source} onChange={setSource} />
        </div>

        <div className="flex h-full flex-col overflow-hidden">
          <Tabs activeTab={mode} onTabChange={setMode} />
          <div className="flex-1 overflow-auto p-5">
            {mode === "cst" && <CstPanel output={parseResult?.output ?? ""} />}
            {mode === "typechecker" && <TypeCheckerPanel data={checkResult} />}
          </div>
        </div>
      </main>
    </div>
  );
}
