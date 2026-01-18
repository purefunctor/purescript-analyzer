import { useEffect } from "react";
import type { Remote } from "comlink";
import { useLocation } from "wouter";
import { useEditorState } from "../hooks/useEditorState";
import { MonacoEditor } from "./Editor/MonacoEditor";
import { Tabs } from "./Tabs";
import { CstPanel } from "./CstPanel";
import { TypeCheckerPanel } from "./TypeCheckerPanel";
import { GetStartedPanel } from "./GetStartedPanel";
import { PackagePanel } from "./PackagePanel";
import type { Lib, Mode, Timing } from "../lib/types";
import type { PackageSet, PackageLoadProgress } from "../lib/packages/types";

interface Props {
  mode: Mode;
  exampleId: string | undefined;
  docsLib: Remote<Lib>;
  onTimingChange: (timing: Timing | null) => void;
  // Package props
  packageSet: PackageSet | null;
  loadProgress: PackageLoadProgress | null;
  loadedPackages: string[];
  onAddPackage: (name: string) => void;
  onClearPackages: () => void;
  isLoadingPackages: boolean;
  packageError: string | null;
  onDismissPackageError: () => void;
}

export function Workspace({
  mode,
  exampleId,
  docsLib,
  onTimingChange,
  packageSet,
  loadProgress,
  loadedPackages,
  onAddPackage,
  onClearPackages,
  isLoadingPackages,
  packageError,
  onDismissPackageError,
}: Props) {
  const [, navigate] = useLocation();

  const { source, cst, typeChecker, timing, selectExample, pendingNavigation } = useEditorState({
    exampleId,
    docsLib,
    mode,
  });

  // Sync timing to parent for header display
  useEffect(() => {
    onTimingChange(timing);
  }, [timing, onTimingChange]);

  // Handle pending navigation from prefetch
  useEffect(() => {
    if (pendingNavigation) {
      navigate(`/types/${pendingNavigation.exampleId}`);
    }
  }, [pendingNavigation, navigate]);

  return (
    <main className="grid flex-1 grid-cols-2 gap-0 overflow-hidden">
      <div className="h-full overflow-hidden border-r border-bg-lighter">
        <MonacoEditor value={source.value} onChange={source.set} />
      </div>

      <div className="flex h-full flex-col overflow-hidden">
        <Tabs activeTab={mode} exampleId={exampleId} />
        <div className="flex-1 overflow-auto p-5">
          {mode === "getstarted" && <GetStartedPanel onSelectExample={selectExample} />}
          {mode === "cst" && <CstPanel output={cst.result?.output ?? ""} />}
          {mode === "typechecker" && (
            <TypeCheckerPanel data={typeChecker.result} loading={typeChecker.isLoading} />
          )}
          {mode === "packages" && (
            <PackagePanel
              packageSet={packageSet}
              loadProgress={loadProgress}
              loadedPackages={loadedPackages}
              onAddPackage={onAddPackage}
              onClearPackages={onClearPackages}
              isLoading={isLoadingPackages}
              error={packageError}
              onDismissError={onDismissPackageError}
            />
          )}
        </div>
      </div>
    </main>
  );
}
