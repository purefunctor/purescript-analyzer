import * as Comlink from "comlink";
import init, * as docsLib from "docs-lib";
import type { ParseResult, CheckResult } from "../lib/types";
import type {
  PackageSet,
  LoadedPackage,
  PackageLoadProgress,
  PackageStatus,
} from "../lib/packages/types";
import { fetchPackage, fetchPackageSet } from "../lib/packages/fetcher";
import { resolveTransitiveDependencies } from "../lib/packages/resolver";

const lib = {
  async init() {
    await init();
  },

  async parse(source: string): Promise<ParseResult> {
    const { output, lex, layout, parse } = docsLib.parse(source);
    return { output, lex, layout, parse };
  },

  async check(source: string): Promise<CheckResult> {
    return docsLib.check(source) as CheckResult;
  },

  async fetchPackageSet(): Promise<PackageSet> {
    return fetchPackageSet();
  },

  async loadPackages(
    packageSet: PackageSet,
    packageNames: string[],
    onProgress: (progress: PackageLoadProgress) => void
  ): Promise<LoadedPackage[]> {
    const deps = resolveTransitiveDependencies(packageSet, packageNames);
    const progress: PackageLoadProgress = {
      packages: new Map(),
      totalPackages: deps.all.length,
      completedPackages: 0,
    };

    // Initialize all as pending
    for (const pkg of deps.all) {
      progress.packages.set(pkg, { state: "pending" });
    }
    onProgress(progress);

    const loaded: LoadedPackage[] = [];

    // Fetch in batches (parallel within batch, serial between batches)
    const BATCH_SIZE = 4;
    for (let i = 0; i < deps.all.length; i += BATCH_SIZE) {
      const batch = deps.all.slice(i, i + BATCH_SIZE);

      const results = await Promise.all(
        batch.map(async (pkgName) => {
          const entry = packageSet[pkgName];
          progress.packages.set(pkgName, { state: "downloading", progress: 0 });
          onProgress({ ...progress, packages: new Map(progress.packages) });

          try {
            const modules = await fetchPackage(pkgName, entry.version, (p) => {
              progress.packages.set(pkgName, { state: "downloading", progress: p });
              onProgress({ ...progress, packages: new Map(progress.packages) });
            });

            progress.packages.set(pkgName, { state: "extracting" });
            onProgress({ ...progress, packages: new Map(progress.packages) });

            // Register modules with WASM engine
            for (const mod of modules) {
              docsLib.register_module(mod.name, mod.source);
            }

            progress.packages.set(pkgName, { state: "ready", moduleCount: modules.length });
            progress.completedPackages++;
            onProgress({ ...progress, packages: new Map(progress.packages) });

            return {
              name: pkgName,
              version: entry.version,
              modules,
              loadedAt: Date.now(),
            };
          } catch (e) {
            const status: PackageStatus = {
              state: "error",
              message: e instanceof Error ? e.message : "Unknown error",
            };
            progress.packages.set(pkgName, status);
            onProgress({ ...progress, packages: new Map(progress.packages) });
            return null;
          }
        })
      );

      loaded.push(...results.filter((r): r is LoadedPackage => r !== null));
    }

    return loaded;
  },

  async clearPackages(): Promise<void> {
    docsLib.clear_packages();
  },

  async registerModule(moduleName: string, source: string): Promise<void> {
    docsLib.register_module(moduleName, source);
  },
};

Comlink.expose(lib);
