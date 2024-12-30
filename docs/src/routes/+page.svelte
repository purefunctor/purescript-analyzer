<script lang="ts">
  import { writable } from "svelte/store";
  import { createDocsLib } from "$lib";
  import { onMount } from "svelte";
  import type { Remote } from "comlink";
  import type { Lib } from "$lib/worker/docs-lib";

  let docsLib: Remote<Lib> | null = null;

  let source = writable("");
  let time = $state(0);
  let output = $state("");

  onMount(async () => {
    docsLib = await createDocsLib();
  });

  onMount(() => {
    return source.subscribe(async (source) => {
      const start = performance.now();
      output = await docsLib!.parse(source);
      time = performance.now() - start;
    });
  });
</script>

<div class="container">
  <div>
    <h1>PureScript Analyzer</h1>
    <p>Finished: {time}ms</p>
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
</style>
