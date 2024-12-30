import * as Comlink from "comlink";
import type { Lib } from "./worker/docs-lib";

export async function createDocsLib() {
  const module = await import("./worker/docs-lib?worker");
  const worker = new module.default();
  const remote = Comlink.wrap<Lib>(worker);
  await remote.init();
  return remote;
}
