import { EXAMPLES, CATEGORIES } from "../lib/examples";

interface Props {
  onSelectExample: (source: string) => void;
}

export function GetStartedPanel({ onSelectExample }: Props) {
  return (
    <div className="space-y-8">
      <div>
        <h2 className="text-lg font-semibold">Welcome to PureScript Analyzer</h2>
        <p className="mt-2 text-sm text-fg-muted">
          Explore PureScript's type system interactively. Select an example below to load it into
          the editor, then switch to the Type Checker tab to see inferred types.
        </p>
      </div>

      {CATEGORIES.map((category) => (
        <section key={category}>
          <h3 className="mb-4 text-xs font-semibold uppercase tracking-wider text-fg-subtle">
            {category}
          </h3>
          <div className="grid grid-cols-1 gap-3 sm:grid-cols-2">
            {EXAMPLES.filter((e) => e.category === category).map((example) => (
              <button
                key={example.id}
                onClick={() => onSelectExample(example.source)}
                className="group rounded-lg border border-bg-lighter bg-bg-lighter/30 p-4 text-left transition-colors hover:border-teal-400/50 hover:bg-bg-lighter"
              >
                <h4 className="font-medium text-fg group-hover:text-teal-400">{example.title}</h4>
                <p className="mt-1 text-sm text-fg-muted">{example.description}</p>
              </button>
            ))}
          </div>
        </section>
      ))}
    </div>
  );
}
