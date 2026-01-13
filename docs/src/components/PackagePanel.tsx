import { useState, useCallback } from "react";
import type { PackageSet, PackageLoadProgress, PackageStatus } from "../lib/packages/types";

interface Props {
  packageSet: PackageSet | null;
  loadProgress: PackageLoadProgress | null;
  loadedPackages: string[];
  onAddPackage: (name: string) => void;
  onClearPackages: () => void;
  isLoading: boolean;
}

function StatusBadge({ status }: { status: PackageStatus }) {
  switch (status.state) {
    case "pending":
      return <span className="text-fg-subtle">Pending</span>;
    case "downloading":
      return <span className="text-blue-400">{Math.round(status.progress * 100)}%</span>;
    case "extracting":
      return <span className="text-yellow-400">Extracting...</span>;
    case "ready":
      return (
        <span className="text-green-400">
          {status.moduleCount} module{status.moduleCount !== 1 ? "s" : ""}
        </span>
      );
    case "error":
      return (
        <span className="text-red-400" title={status.message}>
          Error
        </span>
      );
  }
}

export function PackagePanel({
  packageSet,
  loadProgress,
  loadedPackages,
  onAddPackage,
  onClearPackages,
  isLoading,
}: Props) {
  const [input, setInput] = useState("");
  const [suggestions, setSuggestions] = useState<string[]>([]);
  const [showSuggestions, setShowSuggestions] = useState(false);

  const handleInputChange = useCallback(
    (value: string) => {
      setInput(value);
      if (packageSet && value.length > 1) {
        const matches = Object.keys(packageSet)
          .filter((p) => p.toLowerCase().includes(value.toLowerCase()))
          .filter((p) => !loadedPackages.includes(p))
          .slice(0, 8);
        setSuggestions(matches);
        setShowSuggestions(matches.length > 0);
      } else {
        setSuggestions([]);
        setShowSuggestions(false);
      }
    },
    [packageSet, loadedPackages]
  );

  const handleAddPackage = useCallback(
    (pkg: string) => {
      if (pkg && packageSet && packageSet[pkg]) {
        onAddPackage(pkg);
        setInput("");
        setSuggestions([]);
        setShowSuggestions(false);
      }
    },
    [packageSet, onAddPackage]
  );

  const handleKeyDown = useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key === "Enter" && input) {
        handleAddPackage(input);
      } else if (e.key === "Escape") {
        setShowSuggestions(false);
      }
    },
    [input, handleAddPackage]
  );

  return (
    <div className="space-y-4">
      <h3 className="text-xs font-semibold uppercase tracking-wider text-fg-subtle">Packages</h3>

      {/* Search/Add Package */}
      <div className="relative">
        <input
          type="text"
          value={input}
          onChange={(e) => handleInputChange(e.target.value)}
          onKeyDown={handleKeyDown}
          onFocus={() => suggestions.length > 0 && setShowSuggestions(true)}
          onBlur={() => setTimeout(() => setShowSuggestions(false), 150)}
          placeholder="Add package (e.g., prelude)"
          className="w-full rounded bg-bg-lighter px-3 py-2 text-sm text-fg placeholder-fg-subtle outline-none focus:ring-1 focus:ring-teal-400/50"
          disabled={!packageSet || isLoading}
        />
        {showSuggestions && (
          <div className="absolute left-0 right-0 top-full z-10 mt-1 max-h-48 overflow-auto rounded bg-bg-lighter shadow-lg">
            {suggestions.map((pkg) => (
              <button
                key={pkg}
                onMouseDown={() => handleAddPackage(pkg)}
                className="block w-full px-3 py-2 text-left text-sm hover:bg-bg-lightest"
              >
                {pkg}{" "}
                <span className="text-fg-subtle">@{packageSet![pkg].version.replace(/^v/, "")}</span>
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Progress - only show while actively loading */}
      {isLoading && loadProgress && loadProgress.totalPackages > 0 && (
        <div>
          <div className="mb-1 text-xs text-fg-subtle">
            Loading {loadProgress.completedPackages}/{loadProgress.totalPackages} packages
          </div>
          <div className="h-1.5 overflow-hidden rounded bg-bg-lighter">
            <div
              className="h-full bg-teal-400 transition-all"
              style={{
                width: `${(loadProgress.completedPackages / loadProgress.totalPackages) * 100}%`,
              }}
            />
          </div>
        </div>
      )}

      {/* Loaded Packages List */}
      {loadProgress && loadProgress.packages.size > 0 && (
        <div>
          <div className="mb-2 text-xs text-fg-subtle">
            {loadProgress.packages.size} package{loadProgress.packages.size !== 1 ? "s" : ""} loaded
          </div>
          <div className="max-h-64 space-y-1.5 overflow-y-auto rounded border border-bg-lighter p-2">
            {Array.from(loadProgress.packages.entries()).map(([name, status]) => (
              <div key={name} className="flex items-center justify-between text-sm">
                <span className="text-fg">{name}</span>
                <StatusBadge status={status} />
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Clear Button */}
      {loadedPackages.length > 0 && !isLoading && (
        <button
          onClick={onClearPackages}
          className="text-xs text-red-400 hover:text-red-300"
          disabled={isLoading}
        >
          Clear all packages
        </button>
      )}

      {/* Empty state */}
      {loadedPackages.length === 0 && !isLoading && (
        <div className="text-sm text-fg-muted">
          No packages loaded. Add packages to import modules from the registry.
        </div>
      )}
    </div>
  );
}
