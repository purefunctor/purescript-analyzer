import { useState, useEffect, useCallback, useRef } from "react";
import { useRoute } from "wouter";
import * as Comlink from "comlink";
import { useDocsLib } from "./hooks/useDocsLib";
import { PerformanceBar } from "./components/PerformanceBar";
import { ThemeSwitcher } from "./components/ThemeSwitcher";
import { Workspace } from "./components/Workspace";
import type { Mode, Timing } from "./lib/types";
import type { PackageSet, PackageLoadProgress } from "./lib/packages/types";
import {
  loadCachedPackageSet,
  savePackageSetToCache,
  loadCachedPackages,
  saveCachedPackages,
  clearCachedPackages,
} from "./lib/packages/cache";

export default function App() {
  const docsLib = useDocsLib();

  // Derive mode and exampleId from routes
  const [isHome] = useRoute("/");
  const [isCst] = useRoute("/cst");
  const [isCstExample, cstParams] = useRoute("/cst/:exampleId");
  const [isTypes] = useRoute("/types");
  const [isTypesExample, typesParams] = useRoute("/types/:exampleId");
  const [isPackages] = useRoute("/packages");

  const mode: Mode = isHome
    ? "getstarted"
    : isCst || isCstExample
      ? "cst"
      : isTypes || isTypesExample
        ? "typechecker"
        : isPackages
          ? "packages"
          : "getstarted";

  const exampleId = cstParams?.exampleId || typesParams?.exampleId;

  // Timing state (displayed in header, updated by Workspace)
  const [timing, setTiming] = useState<Timing | null>(null);

  // Package state (global, persists across example changes)
  const [packageSet, setPackageSet] = useState<PackageSet | null>(null);
  const [loadProgress, setLoadProgress] = useState<PackageLoadProgress | null>(null);
  const [loadedPackages, setLoadedPackages] = useState<string[]>([]);
  const [isLoadingPackages, setIsLoadingPackages] = useState(false);
  const [packageError, setPackageError] = useState<string | null>(null);

  const loadingRef = useRef(false);
  const hasRestoredRef = useRef(false);

  // Load package set on mount
  useEffect(() => {
    if (docsLib.status !== "ready") return;
    const lib = docsLib.lib;

    const loadPackageSet = async () => {
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
    if (docsLib.status !== "ready" || !packageSet || hasRestoredRef.current) return;
    hasRestoredRef.current = true;
    const lib = docsLib.lib;

    const restoreCachedPackages = async () => {
      const cached = loadCachedPackages();
      if (cached.size === 0) return;

      setIsLoadingPackages(true);
      const packageNames = Array.from(cached.keys());
      const progressProxy = Comlink.proxy((progress: PackageLoadProgress) =>
        setLoadProgress(progress)
      );

      try {
        const loaded = await lib.loadPackages(packageSet, packageNames, progressProxy);
        setLoadedPackages(loaded.map((p) => p.name));
      } catch (e) {
        console.error("Failed to restore cached packages:", e);
        setPackageError("Failed to restore cached packages");
      } finally {
        setIsLoadingPackages(false);
      }
    };

    restoreCachedPackages();
  }, [docsLib, packageSet]);

  const handleAddPackage = useCallback(
    async (packageName: string) => {
      if (!packageSet || docsLib.status !== "ready" || loadingRef.current) return;
      loadingRef.current = true;

      setIsLoadingPackages(true);
      setPackageError(null);
      const packagesToLoad = [...loadedPackages, packageName];
      const progressProxy = Comlink.proxy((progress: PackageLoadProgress) =>
        setLoadProgress(progress)
      );

      try {
        const loaded = await docsLib.lib.loadPackages(
          packageSet,
          packagesToLoad,
          progressProxy
        );

        const loadedNames = loaded.map((p) => p.name);
        setLoadedPackages(loadedNames);
        saveCachedPackages(new Map(loaded.map((p) => [p.name, p])));
      } catch (e) {
        console.error("Failed to load package:", e);
        setPackageError(e instanceof Error ? e.message : "Failed to load package");
      } finally {
        loadingRef.current = false;
        setIsLoadingPackages(false);
      }
    },
    [packageSet, loadedPackages, docsLib]
  );

  const handleClearPackages = useCallback(async () => {
    if (docsLib.status !== "ready") return;

    await docsLib.lib.clearPackages();
    setLoadedPackages([]);
    setLoadProgress(null);
    clearCachedPackages();
  }, [docsLib]);

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

      <Workspace
        mode={mode}
        exampleId={exampleId}
        docsLib={docsLib.lib}
        onTimingChange={setTiming}
        packageSet={packageSet}
        loadProgress={loadProgress}
        loadedPackages={loadedPackages}
        onAddPackage={handleAddPackage}
        onClearPackages={handleClearPackages}
        isLoadingPackages={isLoadingPackages}
        packageError={packageError}
        onDismissPackageError={() => setPackageError(null)}
      />
    </div>
  );
}
