import RiLoader4Line from "~icons/ri/loader-4-line";
import type { CheckResult } from "../lib/types";
import { HighlightedCode } from "./HighlightedCode";

interface Props {
  data: CheckResult | null;
  loading?: boolean;
}

export function TypeCheckerPanel({ data, loading }: Props) {
  // Only show spinner when explicitly loading (after delay threshold)
  // No data + not loading = waiting quietly, show nothing
  if (loading) {
    return (
      <div className="flex h-full items-center justify-center text-fg-muted">
        <RiLoader4Line className="h-5 w-5 animate-spin" />
      </div>
    );
  }

  if (!data) {
    return null;
  }

  return (
    <div className="space-y-8">
      {data.terms.length > 0 && (
        <section>
          <h3 className="mb-3 text-xs font-semibold uppercase tracking-wider text-fg-subtle">
            Term Signatures
          </h3>
          <div className="space-y-1.5">
            {data.terms.map((sig, i) => (
              <HighlightedCode key={i} code={sig} />
            ))}
          </div>
        </section>
      )}

      {data.types.length > 0 && (
        <section>
          <h3 className="mb-3 text-xs font-semibold uppercase tracking-wider text-fg-subtle">
            Type Signatures
          </h3>
          <div className="space-y-1.5">
            {data.types.map((sig, i) => (
              <HighlightedCode key={i} code={sig} />
            ))}
          </div>
        </section>
      )}

      {data.synonyms.length > 0 && (
        <section>
          <h3 className="mb-3 text-xs font-semibold uppercase tracking-wider text-fg-subtle">
            Type Synonyms
          </h3>
          <div className="space-y-3">
            {data.synonyms.map((syn, i) => (
              <div key={i} className="font-mono text-base">
                <div>
                  <span className="text-teal-400">{syn.name}</span>
                  <span className="text-fg-subtle"> = </span>
                  <span className="text-fg">{syn.expansion}</span>
                </div>
                <div className="mt-1 ml-4 text-xs text-fg-subtle">
                  Quantified: {syn.quantified_variables}, Kind: {syn.kind_variables}, Type:{" "}
                  {syn.type_variables}
                </div>
              </div>
            ))}
          </div>
        </section>
      )}

      {data.errors.length > 0 && (
        <section>
          <h3 className="mb-3 text-xs font-semibold uppercase tracking-wider text-red-400">
            Errors ({data.errors.length})
          </h3>
          <div className="space-y-3">
            {data.errors.map((error, i) => (
              <div
                key={i}
                className="rounded-md border border-red-400/30 bg-red-400/10 px-3 py-2.5"
              >
                <div className="text-sm font-medium text-red-400">{error.kind}</div>
                <div className="mt-1.5 text-sm text-fg-muted">{error.message}</div>
                {error.location && (
                  <div className="mt-1.5 text-xs text-fg-subtle">at {error.location}</div>
                )}
              </div>
            ))}
          </div>
        </section>
      )}

      {data.terms.length === 0 &&
        data.types.length === 0 &&
        data.synonyms.length === 0 &&
        data.errors.length === 0 && (
          <div className="text-fg-muted">No type information available</div>
        )}
    </div>
  );
}
