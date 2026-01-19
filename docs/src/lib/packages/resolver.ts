import type { PackageSet } from "./types";

export interface DependencyTree {
  direct: string[];
  transitive: string[];
  all: string[]; // direct + transitive, topologically sorted
}

/**
 * Compute transitive closure of dependencies.
 * Returns packages in topological order (dependencies before dependents).
 */
export function resolveTransitiveDependencies(
  packageSet: PackageSet,
  requestedPackages: string[]
): DependencyTree {
  const visited = new Set<string>();
  const result: string[] = [];

  function visit(pkg: string) {
    if (visited.has(pkg)) return;
    visited.add(pkg);

    const entry = packageSet[pkg];
    if (!entry) {
      console.warn(`Package not found in set: ${pkg}`);
      return;
    }

    // Visit dependencies first (topological sort)
    for (const dep of entry.dependencies) {
      visit(dep);
    }

    result.push(pkg);
  }

  for (const pkg of requestedPackages) {
    visit(pkg);
  }

  const directSet = new Set(requestedPackages);
  return {
    direct: requestedPackages.filter((p) => packageSet[p]),
    transitive: result.filter((p) => !directSet.has(p)),
    all: result,
  };
}
