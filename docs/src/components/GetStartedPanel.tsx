import RiMergeLine from "~icons/ri/git-merge-line";
import RiStackLine from "~icons/ri/stack-line";
import RiListCheck2 from "~icons/ri/list-check-2";
import RiText from "~icons/ri/text";
import RiCalculatorLine from "~icons/ri/calculator-line";
import RiDnaLine from "~icons/ri/dna-line";
import RiBox3Line from "~icons/ri/box-3-line";
import RiShapesLine from "~icons/ri/shapes-line";
import RiArrowRightLine from "~icons/ri/arrow-right-line";
import RiLinksLine from "~icons/ri/links-line";
import RiMagicLine from "~icons/ri/magic-line";
import RiSparklingLine from "~icons/ri/sparkling-line";
import RiScales3Line from "~icons/ri/scales-3-line";
import RiLoopLeftLine from "~icons/ri/loop-left-line";
import { EXAMPLES, CATEGORIES, type Example } from "../lib/examples";

const ICONS: Record<string, React.ComponentType<{ className?: string }>> = {
  merge: RiMergeLine,
  layers: RiStackLine,
  list: RiListCheck2,
  "text-fields": RiText,
  calculator: RiCalculatorLine,
  dna: RiDnaLine,
  box: RiBox3Line,
  cube: RiShapesLine,
  arrow: RiArrowRightLine,
  link: RiLinksLine,
  wand: RiMagicLine,
  sparkles: RiSparklingLine,
  scale: RiScales3Line,
  loop: RiLoopLeftLine,
};

interface ExampleCardProps {
  example: Example;
  onSelect: (exampleId: string) => void;
}

function ExampleCard({ example, onSelect }: ExampleCardProps) {
  const Icon = ICONS[example.icon];

  return (
    <button
      onClick={() => onSelect(example.id)}
      className="group flex h-full cursor-pointer flex-col rounded-xl border border-bg-lighter bg-bg-lighter/20 p-5 text-left transition-all hover:border-[#cba6f7]/50 hover:bg-bg-lighter/40 hover:shadow-lg hover:shadow-[#cba6f7]/5"
    >
      <div className="mb-3 flex h-10 w-10 items-center justify-center rounded-lg bg-[#cba6f7]/10 text-[#cba6f7] transition-colors group-hover:bg-[#cba6f7]/20">
        {Icon && <Icon className="h-5 w-5" />}
      </div>
      <h4 className="font-medium text-fg transition-colors group-hover:text-[#cba6f7]">
        {example.title}
      </h4>
      <p className="mt-2 flex-1 text-sm leading-relaxed text-fg-muted">{example.description}</p>
    </button>
  );
}

interface GetStartedPanelProps {
  onSelectExample: (exampleId: string) => void;
}

export function GetStartedPanel({ onSelectExample }: GetStartedPanelProps) {
  return (
    <div className="space-y-10">
      <div className="max-w-2xl">
        <h2 className="text-xl font-semibold mb-3">PureScript Analyzer Playground</h2>
        <p className="leading-relaxed text-fg-muted">
          Select an example below to load it into the editor.
        </p>
        <p className="leading-relaxed text-fg-muted">
          You can also load registry packages in the packages tab.
        </p>
      </div>

      {CATEGORIES.map((category) => (
        <section key={category}>
          <h3 className="mb-5 text-xs font-semibold uppercase tracking-wider text-fg-subtle">
            {category}
          </h3>
          <div className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
            {EXAMPLES.filter((e) => e.category === category).map((example) => (
              <ExampleCard key={example.id} example={example} onSelect={onSelectExample} />
            ))}
          </div>
        </section>
      ))}
    </div>
  );
}
