import { useState, useEffect, useCallback } from "react";
import * as Comlink from "comlink";
import { useDocsLib } from "./hooks/useDocsLib";
import { useDebounce } from "./hooks/useDebounce";
import { MonacoEditor } from "./components/Editor/MonacoEditor";
import { Tabs } from "./components/Tabs";
import { CstPanel } from "./components/CstPanel";
import { TypeCheckerPanel } from "./components/TypeCheckerPanel";
import { PerformanceBar } from "./components/PerformanceBar";
import { ThemeSwitcher } from "./components/ThemeSwitcher";
import { PackagePanel } from "./components/PackagePanel";
import { GetStartedPanel } from "./components/GetStartedPanel";
import type { ParseResult, CheckResult, Mode, Timing } from "./lib/types";
import type { PackageSet, PackageLoadProgress } from "./lib/packages/types";
import {
  loadCachedPackageSet,
  savePackageSetToCache,
  loadCachedPackages,
  saveCachedPackages,
  clearCachedPackages,
} from "./lib/packages/cache";

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
  const [mode, setMode] = useState<Mode>("getstarted");
  const [parseResult, setParseResult] = useState<ParseResult | null>(null);
  const [checkResult, setCheckResult] = useState<CheckResult | null>(null);
  const [timing, setTiming] = useState<Timing | null>(null);

  // Package state
  const [packageSet, setPackageSet] = useState<PackageSet | null>(null);
  const [loadProgress, setLoadProgress] = useState<PackageLoadProgress | null>(null);
  const [loadedPackages, setLoadedPackages] = useState<string[]>([]);
  const [isLoadingPackages, setIsLoadingPackages] = useState(false);

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
        total: parsed.lex + parsed.layout + parsed.parse,
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
          total: checked.timing.total,
        });
      }
    } catch (err) {
      console.error("Analysis error:", err);
    }
  }, [docsLib, debouncedSource, mode]);

  useEffect(() => {
    runAnalysis();
  }, [runAnalysis]);

  // Load package set on mount
  useEffect(() => {
    if (docsLib.status !== "ready") return;
    const lib = docsLib.lib;

    const loadPackageSet = async () => {
      // Try cache first
      const cached = loadCachedPackageSet();
      if (cached) {
        setPackageSet(cached);
        return;
      }

      try {
        const ps = await lib.fetchPackageSet();
        setPackageSet(ps);
        savePackageSetToCache(ps);
      } catch (e) {
        console.error("Failed to fetch package set:", e);
      }
    };

    loadPackageSet();
  }, [docsLib]);

  // Restore cached packages on mount
  useEffect(() => {
    if (docsLib.status !== "ready" || !packageSet) return;
    const lib = docsLib.lib;

    const restoreCachedPackages = async () => {
      const cached = loadCachedPackages();
      if (cached.size === 0) return;

      setIsLoadingPackages(true);
      const packageNames = Array.from(cached.keys());

      try {
        const loaded = await lib.loadPackages(
          packageSet,
          packageNames,
          Comlink.proxy((progress) => setLoadProgress(progress))
        );
        setLoadedPackages(loaded.map((p) => p.name));
      } catch (e) {
        console.error("Failed to restore cached packages:", e);
      } finally {
        setIsLoadingPackages(false);
      }
    };

    restoreCachedPackages();
  }, [docsLib, packageSet]);

  // Package handlers
  const handleAddPackage = useCallback(
    async (packageName: string) => {
      if (!packageSet || docsLib.status !== "ready" || isLoadingPackages) return;

      setIsLoadingPackages(true);
      const packagesToLoad = [...loadedPackages, packageName];

      try {
        const loaded = await docsLib.lib.loadPackages(
          packageSet,
          packagesToLoad,
          Comlink.proxy((progress) => setLoadProgress(progress))
        );

        const loadedNames = loaded.map((p) => p.name);
        setLoadedPackages(loadedNames);
        saveCachedPackages(new Map(loaded.map((p) => [p.name, p])));
      } catch (e) {
        console.error("Failed to load package:", e);
      } finally {
        setIsLoadingPackages(false);
      }
    },
    [packageSet, loadedPackages, docsLib, isLoadingPackages]
  );

  const handleClearPackages = useCallback(async () => {
    if (docsLib.status !== "ready") return;

    await docsLib.lib.clearPackages();
    setLoadedPackages([]);
    setLoadProgress(null);
    clearCachedPackages();
  }, [docsLib]);

  const handleSelectExample = useCallback((source: string) => {
    setSource(source);
    setMode("typechecker");
  }, []);

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
            {mode === "getstarted" && <GetStartedPanel onSelectExample={handleSelectExample} />}
            {mode === "cst" && <CstPanel output={parseResult?.output ?? ""} />}
            {mode === "typechecker" && <TypeCheckerPanel data={checkResult} />}
            {mode === "packages" && (
              <PackagePanel
                packageSet={packageSet}
                loadProgress={loadProgress}
                loadedPackages={loadedPackages}
                onAddPackage={handleAddPackage}
                onClearPackages={handleClearPackages}
                isLoading={isLoadingPackages}
              />
            )}
          </div>
        </div>
      </main>
    </div>
  );
}
