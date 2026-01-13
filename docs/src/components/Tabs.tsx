import type { Mode } from "../lib/types";

interface Props {
  activeTab: Mode;
  onTabChange: (tab: Mode) => void;
}

const tabs: { id: Mode; label: string }[] = [
  { id: "getstarted", label: "Get Started" },
  { id: "typechecker", label: "Type Checker" },
  { id: "cst", label: "CST Preview" },
  { id: "packages", label: "Packages" },
];

export function Tabs({ activeTab, onTabChange }: Props) {
  return (
    <div className="flex border-b border-bg-lighter">
      {tabs.map((tab) => (
        <button
          key={tab.id}
          onClick={() => onTabChange(tab.id)}
          className={`px-4 py-2.5 text-sm font-medium tracking-wide transition-colors ${
            activeTab === tab.id
              ? "border-b-2 border-fg text-fg"
              : "text-fg-subtle hover:text-fg-muted"
          }`}
        >
          {tab.label}
        </button>
      ))}
    </div>
  );
}
