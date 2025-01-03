<script lang="ts">
  import { writable } from "svelte/store";
  import { createDocsLib } from "$lib";
  import { onMount } from "svelte";
  import type { Remote } from "comlink";
  import type { Lib } from "$lib/worker/docs-lib";

  let docsLib: Remote<Lib> | null = null;

  let source = writable("");
  let lex = $state(0);
  let layout = $state(0);
  let parse = $state(0);
  let output = $state("");

  onMount(async () => {
    docsLib = await createDocsLib();
  });

  onMount(() => {
    return source.subscribe(async (source) => {
      if (!docsLib) return;
      let result = await docsLib!.parse(source);
      lex = result.lex;
      layout = result.layout;
      parse = result.parse;
      output = result.output;
    });
  });
</script>

<div class="container">
  <div>
    <h1>PureScript Analyzer</h1>
    <p>Lex: {lex}ms</p>
    <p>Layout: {layout}ms</p>
    <p>Parse: {parse}ms</p>
  </div>
  <div class="grid">
    <div class="editor">
      <textarea bind:value={$source}></textarea>
    </div>
    <div class="preview">
      <pre><code>{output}</code></pre>
    </div>
  </div>
</div>

<style>
  :global body {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }

  .container {
    display: flex;
    flex-direction: column;
    box-sizing: border-box;
    padding: 1rem;
    height: 100vh;
  }

  .container h1 {
    margin: 0;
  }

  .grid {
    min-height: 0;
    height: 100%;
    display: grid;
    grid-template-columns: 1fr 1fr;
    grid-template-rows: minmax(0, 1fr);
    gap: 1rem;
  }

  .editor textarea {
    width: 100%;
    height: 100%;
  }

  .preview {
    overflow-y: scroll;
    display: flex;
    flex-direction: column-reverse;
  }

  .preview > pre {
    flex-grow: 1;
  }
</style>
