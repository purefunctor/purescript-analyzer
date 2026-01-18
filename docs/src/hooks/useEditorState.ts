import { useReducer, useEffect, useCallback, useRef } from "react";
import type { Remote } from "comlink";
import { useDebounce } from "./useDebounce";
import { EXAMPLES } from "../lib/examples";
import type { Lib, ParseResult, CheckResult, Timing } from "../lib/types";

const DEFAULT_SOURCE = `module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

deriveUnion :: forall u. Row.Union (a :: Int) (b :: String) u => Proxy u
deriveUnion = Proxy

deriveUnionLeft :: forall l. Row.Union l (b :: String) (a :: Int, b :: String) => Proxy l
deriveUnionLeft = Proxy

solveUnion = { deriveUnion, deriveUnionLeft }
`;

// How long to wait before navigating without results (shows spinner)
const PREFETCH_TIMEOUT_MS = 100;
// Debounce delay for user typing
const TYPING_DEBOUNCE_MS = 150;

interface Results {
  cst: ParseResult;
  typeChecker: CheckResult | null;
  timing: Timing;
}

// State machine for proactive navigation
type State =
  | { status: "idle"; source: string; results: Results }
  | { status: "prefetching"; source: string; results: Results | null; targetExampleId: string }
  | { status: "loading"; source: string; results: Results | null }
  | { status: "stale"; source: string; results: Results };

type Action =
  | { type: "SELECT_EXAMPLE"; exampleId: string; source: string }
  | { type: "PREFETCH_TIMEOUT" }
  | { type: "SOURCE_EDITED"; source: string }
  | { type: "ANALYSIS_COMPLETE"; source: string; results: Results }
  | { type: "URL_LOADED"; source: string };

function reducer(state: State, action: Action): State {
  switch (action.type) {
    case "SELECT_EXAMPLE":
      // Start prefetching - keep existing results while we fetch new ones
      return {
        status: "prefetching",
        source: action.source,
        results: state.status === "idle" || state.status === "stale" ? state.results : null,
        targetExampleId: action.exampleId,
      };

    case "PREFETCH_TIMEOUT":
      // Only handle if we're still prefetching
      if (state.status !== "prefetching") return state;
      // Timeout hit - transition to loading (will show spinner)
      return { status: "loading", source: state.source, results: state.results };

    case "SOURCE_EDITED":
      if (action.source === state.source) return state;
      if (state.status === "idle" || state.status === "stale") {
        return { status: "stale", source: action.source, results: state.results };
      }
      return { ...state, source: action.source };

    case "ANALYSIS_COMPLETE":
      if (action.source !== state.source) return state;
      // Transitions from any state (prefetching, loading, stale) to idle
      return { status: "idle", source: state.source, results: action.results };

    case "URL_LOADED":
      // Direct URL access (back/forward, shared links) - can't prefetch
      if (action.source === state.source && state.status === "idle") return state;
      return { status: "loading", source: action.source, results: null };
  }
}

// Hook options and return types
interface UseEditorStateOptions {
  exampleId: string | undefined;
  docsLib: Remote<Lib>;
  mode: "cst" | "typechecker" | "getstarted" | "packages";
}

interface SourceState {
  value: string;
  set: (value: string) => void;
}

interface AnalysisState<T> {
  isLoading: boolean;
  result: T | null;
}

interface EditorState {
  source: SourceState;
  cst: AnalysisState<ParseResult>;
  typeChecker: AnalysisState<CheckResult>;
  timing: Timing | null;
  selectExample: (exampleId: string) => void;
  pendingNavigation: { exampleId: string } | null;
}

export function useEditorState({
  exampleId,
  docsLib,
  mode,
}: UseEditorStateOptions): EditorState {
  const getSourceForExample = useCallback(
    (id: string | undefined) =>
      id ? (EXAMPLES.find((e) => e.id === id)?.source ?? DEFAULT_SOURCE) : DEFAULT_SOURCE,
    []
  );

  // Initialise with loading state
  const [state, dispatch] = useReducer(reducer, null, () => ({
    status: "loading" as const,
    source: getSourceForExample(exampleId),
    results: null,
  }));

  const loadedExampleRef = useRef(exampleId);
  const prefetchTimeoutRef = useRef<number | null>(null);
  const pendingAnalysisRef = useRef<{ source: string; cancelled: boolean } | null>(null);

  // Handle URL changes (direct navigation: back/forward, shared links, typed URL)
  // vs in-app navigation (selectExample sets source first, then navigate happens)
  useEffect(() => {
    if (exampleId !== loadedExampleRef.current) {
      const expectedSource = getSourceForExample(exampleId);
      // If source doesn't match what the URL expects, this is direct navigation
      if (expectedSource !== state.source) {
        dispatch({ type: "URL_LOADED", source: expectedSource });
      }
      // Update ref regardless - URL now matches
      loadedExampleRef.current = exampleId;
    }
  }, [exampleId, getSourceForExample, state.source]);

  // Debounced source for typing - only used when in stale state
  const debouncedSource = useDebounce(state.source, TYPING_DEBOUNCE_MS);

  // Determine which source to use for analysis:
  // - Immediate (no debounce) for prefetching/loading/initial
  // - Debounced for stale (user is typing)
  const analysisSource = state.status === "stale" ? debouncedSource : state.source;

  // Run analysis when source changes
  useEffect(() => {
    const sourceSnapshot = analysisSource;

    // Cancel any pending analysis
    if (pendingAnalysisRef.current) {
      pendingAnalysisRef.current.cancelled = true;
    }

    const analysisContext = { source: sourceSnapshot, cancelled: false };
    pendingAnalysisRef.current = analysisContext;

    const runAnalysis = async () => {
      try {
        const cst = await docsLib.parse(sourceSnapshot);
        if (analysisContext.cancelled) return;

        let timing: Timing = {
          lex: cst.lex,
          layout: cst.layout,
          parse: cst.parse,
          total: cst.lex + cst.layout + cst.parse,
        };

        let typeChecker: CheckResult | null = null;
        if (mode === "typechecker") {
          typeChecker = await docsLib.check(sourceSnapshot);
          if (analysisContext.cancelled) return;

          timing = {
            lex: typeChecker.timing.lex,
            layout: typeChecker.timing.layout,
            parse: typeChecker.timing.parse,
            stabilize: typeChecker.timing.stabilize,
            index: typeChecker.timing.index,
            resolve: typeChecker.timing.resolve,
            lower: typeChecker.timing.lower,
            check: typeChecker.timing.check,
            total: typeChecker.timing.total,
          };
        }

        if (analysisContext.cancelled) return;

        dispatch({
          type: "ANALYSIS_COMPLETE",
          source: sourceSnapshot,
          results: { cst, typeChecker, timing },
        });
      } catch (err) {
        console.error("Analysis error:", err);
      }
    };

    runAnalysis();

    return () => {
      analysisContext.cancelled = true;
    };
  }, [docsLib, analysisSource, mode]);

  // Handle prefetch timeout
  useEffect(() => {
    if (state.status === "prefetching") {
      prefetchTimeoutRef.current = window.setTimeout(() => {
        dispatch({ type: "PREFETCH_TIMEOUT" });
      }, PREFETCH_TIMEOUT_MS);

      return () => {
        if (prefetchTimeoutRef.current) {
          clearTimeout(prefetchTimeoutRef.current);
          prefetchTimeoutRef.current = null;
        }
      };
    }
  }, [state.status]);

  const setSource = useCallback((newSource: string) => {
    dispatch({ type: "SOURCE_EDITED", source: newSource });
  }, []);

  const selectExample = useCallback(
    (targetExampleId: string) => {
      const source = getSourceForExample(targetExampleId);
      dispatch({ type: "SELECT_EXAMPLE", exampleId: targetExampleId, source });
    },
    [getSourceForExample]
  );

  // Determine pending navigation
  // Navigate when we're no longer prefetching and URL doesn't match the current source
  const pendingNavigation = (() => {
    // Still prefetching - don't navigate yet
    if (state.status === "prefetching") return null;

    // Check if current source corresponds to an example that differs from URL
    const currentExample = EXAMPLES.find((e) => e.source === state.source);
    if (currentExample && currentExample.id !== loadedExampleRef.current) {
      return { exampleId: currentExample.id };
    }

    return null;
  })();

  // Determine loading state - only show spinner for loading state (not prefetching)
  const isLoading = state.status === "loading";
  const results = state.status === "idle" || state.status === "stale" ? state.results : null;

  return {
    source: {
      value: state.source,
      set: setSource,
    },
    cst: {
      isLoading,
      result: results?.cst ?? null,
    },
    typeChecker: {
      isLoading,
      result: results?.typeChecker ?? null,
    },
    timing: results?.timing ?? null,
    selectExample,
    pendingNavigation,
  };
}
