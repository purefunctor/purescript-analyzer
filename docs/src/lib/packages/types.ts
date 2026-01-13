// Package set format from purescript/package-sets
export interface PackageSetEntry {
  version: string;
  repo: string;
  dependencies: string[];
}

export type PackageSet = Record<string, PackageSetEntry>;

// Internal state
export interface PackageModule {
  name: string; // e.g., "Data.Maybe"
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
