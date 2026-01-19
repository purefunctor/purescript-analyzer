// Package set format from purescript/package-sets
export interface PackageSetEntry {
  version: string;
  repo: string;
  dependencies: string[];
}

export type PackageSet = Record<string, PackageSetEntry>;

// Raw module data from tar extraction (before WASM parsing)
export interface RawModule {
  path: string; // tar path, e.g., "prelude-6.0.1/src/Data/Maybe.purs"
  source: string; // PureScript source code
}

// Internal state (after WASM parsing extracts module name)
export interface PackageModule {
  path: string; // tar path, e.g., "prelude-6.0.1/src/Data/Maybe.purs"
  name: string; // module name returned from WASM, e.g., "Data.Maybe"
  source: string; // PureScript source code
}

export interface LoadedPackage {
  name: string;
  version: string;
  modules: PackageModule[];
  loadedAt: number; // timestamp for cache validation
}

export type PackageStatus =
  | { state: "pending" }
  | { state: "downloading"; progress: number }
  | { state: "extracting" }
  | { state: "ready"; moduleCount: number }
  | { state: "error"; message: string };

export interface PackageLoadProgress {
  packages: Map<string, PackageStatus>;
  totalPackages: number;
  completedPackages: number;
}
