<script>
  import { writable } from "svelte/store";
  import init, { parse } from "../wasm/pkg/docs_lib";

  let source = writable("");
  let output = $state("");

  $effect(() => {
    init().then(() => {
      source.subscribe((value) => {
        output = parse(value);
      });
    });
  });
</script>

<div class="container">
  <h1>PureScript Analyzer</h1>
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
