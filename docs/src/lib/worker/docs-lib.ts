import * as Comlink from "comlink";
import init, * as docsLib from "docs-lib";

const lib = {
  async init() {
    await init();
  },
  async parse(source: string) {
    let { output, lex, layout, parse } = docsLib.parse(source);
    return { output, lex, layout, parse };
  },
};

export interface Lib {
  init(): Promise<void>;
  parse(
    source: string
  ): Promise<{ output: string; lex: number; layout: number; parse: number }>;
}

Comlink.expose(lib);
