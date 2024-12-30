import * as Comlink from "comlink";
import init, { parse } from "docs-lib";

const lib = {
  async init() {
    await init();
  },
  async parse(source: string) {
    return parse(source);
  },
};

export interface Lib {
  init(): Promise<void>;
  parse(source: string): Promise<string>;
}

Comlink.expose(lib);
