import * as Comlink from "comlink";
import type { Lib } from "./types";

export async function createDocsLib(): Promise<Comlink.Remote<Lib>> {
  const module = await import("../worker/docs-lib?worker");
  const worker = new module.default();
  const remote = Comlink.wrap<Lib>(worker);
  await remote.init();
  return remote;
}
