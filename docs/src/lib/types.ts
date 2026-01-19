import type { PackageSet, LoadedPackage, PackageLoadProgress } from "./packages/types";

export type Mode = "cst" | "typechecker" | "getstarted" | "packages";

export interface ParseResult {
  output: string;
  lex: number;
  layout: number;
  parse: number;
}

export interface SynonymExpansion {
  name: string;
  expansion: string;
  quantified_variables: number;
  kind_variables: number;
  type_variables: number;
}

export interface CheckErrorInfo {
  kind: string;
  message: string;
  location?: string;
}

export interface CheckTiming {
  lex: number;
  layout: number;
  parse: number;
  stabilize: number;
  index: number;
  resolve: number;
  lower: number;
  check: number;
  total: number;
}

export interface CheckResult {
  terms: string[];
  types: string[];
  synonyms: SynonymExpansion[];
  errors: CheckErrorInfo[];
  timing: CheckTiming;
}

export interface Timing {
  lex: number;
  layout: number;
  parse: number;
  stabilize?: number;
  index?: number;
  resolve?: number;
  lower?: number;
  check?: number;
  total: number;
}

export interface Lib {
  init(): Promise<void>;
  parse(source: string): Promise<ParseResult>;
  check(source: string): Promise<CheckResult>;
  fetchPackageSet(): Promise<PackageSet>;
  loadPackages(
    packageSet: PackageSet,
    packageNames: string[],
    onProgress: (progress: PackageLoadProgress) => void
  ): Promise<LoadedPackage[]>;
  clearPackages(): Promise<void>;
  registerModule(path: string, source: string): Promise<string | undefined>;
}
