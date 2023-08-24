/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
	const serverExecutable = context.asAbsolutePath(
		path.join("..", "target", "debug", "demo-bin"),
	);

	const serverOptions: ServerOptions = {
		run: <Executable>{ command: serverExecutable, transport: TransportKind.stdio },
		debug: <Executable>{ command: serverExecutable, transport: TransportKind.stdio },
	};
	
	const clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "purescript", pattern: "**/*.purs" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/*.purs"),	
		},
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
