import type { LoadedPackage, PackageSet } from "./types";

const STORAGE_KEY_PACKAGES = "purescript-analyzer:packages";
const STORAGE_KEY_PACKAGE_SET = "purescript-analyzer:package-set";
const CACHE_TTL_MS = 24 * 60 * 60 * 1000; // 24 hours

interface CachedPackageSet {
  data: PackageSet;
  fetchedAt: number;
}

interface CachedPackages {
  packages: Record<string, LoadedPackage>;
}

export function loadCachedPackageSet(): PackageSet | null {
  try {
    const raw = localStorage.getItem(STORAGE_KEY_PACKAGE_SET);
    if (!raw) return null;

    const cached: CachedPackageSet = JSON.parse(raw);
    if (Date.now() - cached.fetchedAt > CACHE_TTL_MS) {
      return null; // Expired
    }
    return cached.data;
  } catch {
    return null;
  }
}

export function savePackageSetToCache(packageSet: PackageSet): void {
  const cached: CachedPackageSet = {
    data: packageSet,
    fetchedAt: Date.now(),
  };
  localStorage.setItem(STORAGE_KEY_PACKAGE_SET, JSON.stringify(cached));
}

export function loadCachedPackages(): Map<string, LoadedPackage> {
  try {
    const raw = localStorage.getItem(STORAGE_KEY_PACKAGES);
    if (!raw) return new Map();

    const cached: CachedPackages = JSON.parse(raw);
    return new Map(Object.entries(cached.packages));
  } catch {
    return new Map();
  }
}

export function saveCachedPackages(packages: Map<string, LoadedPackage>): void {
  const cached: CachedPackages = {
    packages: Object.fromEntries(packages),
  };
  localStorage.setItem(STORAGE_KEY_PACKAGES, JSON.stringify(cached));
}

export function clearCachedPackages(): void {
  localStorage.removeItem(STORAGE_KEY_PACKAGES);
}
