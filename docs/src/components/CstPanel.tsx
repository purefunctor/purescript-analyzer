interface Props {
  output: string;
}

export function CstPanel({ output }: Props) {
  if (!output) {
    return (
      <div className="flex h-full items-center justify-center text-fg-muted">
        Enter PureScript code to see the CST
      </div>
    );
  }

  return (
    <pre className="font-mono text-base whitespace-pre-wrap break-words text-fg">
      <code>{output}</code>
    </pre>
  );
}
