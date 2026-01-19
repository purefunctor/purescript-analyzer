import { Link } from "wouter";
import type { Mode } from "../lib/types";

interface Props {
  activeTab: Mode;
  exampleId?: string;
}

const tabs: { id: Mode; label: string; path: string }[] = [
  { id: "getstarted", label: "Get Started", path: "/" },
  { id: "typechecker", label: "Type Checker", path: "/types" },
  { id: "cst", label: "CST Preview", path: "/cst" },
  { id: "packages", label: "Packages", path: "/packages" },
];

export function Tabs({ activeTab, exampleId }: Props) {
  return (
    <div className="flex border-b border-bg-lighter">
      {tabs.map((tab) => {
        // Preserve exampleId for CST and Type Checker tabs
        const path =
          exampleId && (tab.id === "cst" || tab.id === "typechecker")
            ? `${tab.path}/${exampleId}`
            : tab.path;

        return (
          <Link
            key={tab.id}
            href={path}
            className={`px-4 py-2.5 text-sm font-medium tracking-wide transition-colors ${
              activeTab === tab.id
                ? "border-b-2 border-fg text-fg"
                : "text-fg-subtle hover:text-fg-muted"
            }`}
          >
            {tab.label}
          </Link>
        );
      })}
    </div>
  );
}
