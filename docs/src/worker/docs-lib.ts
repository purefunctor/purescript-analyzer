import * as Comlink from "comlink";
import init, * as docsLib from "docs-lib";
import type { ParseResult, CheckResult } from "../lib/types";

const lib = {
  async init() {
    await init();
  },

  async parse(source: string): Promise<ParseResult> {
    const { output, lex, layout, parse } = docsLib.parse(source);
    return { output, lex, layout, parse };
  },

  async check(source: string): Promise<CheckResult> {
    return docsLib.check(source) as CheckResult;
  },
};

Comlink.expose(lib);
