import type { Timing } from "../lib/types";

interface Props {
  timing: Timing;
}

function formatMs(ms: number): string {
  if (ms < 1) {
    return `${(ms * 1000).toFixed(0)}us`;
  }
  return `${ms.toFixed(1)}ms`;
}

export function PerformanceBar({ timing }: Props) {
  const metrics: { label: string; value: number | undefined }[] = [
    { label: "Lex", value: timing.lex },
    { label: "Layout", value: timing.layout },
    { label: "Parse", value: timing.parse },
    { label: "Stabilize", value: timing.stabilize },
    { label: "Index", value: timing.index },
    { label: "Resolve", value: timing.resolve },
    { label: "Lower", value: timing.lower },
    { label: "Check", value: timing.check },
  ].filter((m) => m.value !== undefined);

  return (
    <div className="flex gap-4 text-xs text-fg-subtle">
      {metrics.map((metric) => (
        <div key={metric.label} className="flex items-center gap-1.5">
          <span className="font-medium">{metric.label}:</span>
          <span className="font-mono text-fg-muted">{formatMs(metric.value!)}</span>
        </div>
      ))}
      <div className="flex items-center gap-1.5 border-l border-bg-lighter pl-4">
        <span className="font-medium">Total:</span>
        <span className="font-mono text-fg-muted">{formatMs(timing.total)}</span>
      </div>
    </div>
  );
}
