import { ExtensionContext, workspace } from "vscode";

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const config = workspace.getConfiguration("purescriptAnalyzer");
  const sourceCommand = config.get<string>("sourceCommand");

  const args: string[] = [];
  if (sourceCommand) {
    args.push("--source-command", sourceCommand);
  }

  const serverOptions: ServerOptions = {
    command: "purescript-analyzer",
    args,
    transport: TransportKind.stdio,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "purescript" }],
  };

  client = new LanguageClient(
    "purescript-analyzer",
    "PureScript Analyzer",
    serverOptions,
    clientOptions,
  );

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
