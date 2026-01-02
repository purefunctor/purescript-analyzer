import { useState, useEffect } from "react";
import type { Remote } from "comlink";
import { createDocsLib } from "../lib/worker";
import type { Lib } from "../lib/types";

type DocsLibState =
  | { status: "loading" }
  | { status: "ready"; lib: Remote<Lib> }
  | { status: "error"; error: Error };

export function useDocsLib(): DocsLibState {
  const [state, setState] = useState<DocsLibState>({ status: "loading" });

  useEffect(() => {
    let cancelled = false;

    createDocsLib()
      .then((lib) => {
        if (!cancelled) {
          setState({ status: "ready", lib });
        }
      })
      .catch((error) => {
        if (!cancelled) {
          setState({ status: "error", error });
        }
      });

    return () => {
      cancelled = true;
    };
  }, []);

  return state;
}
