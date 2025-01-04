<script lang="ts">
  import type { Lib } from "$lib/worker/docs-lib";
  import type { Remote } from "comlink";

  let { docsLib }: { docsLib: Remote<Lib> } = $props();

  let source = $state("");
  let output = $state("");
  let lex = $state(0);
  let layout = $state(0);
  let parse = $state(0);

  async function onSourceChange() {
    let result = await docsLib.parse(source);
    output = result.output;
    lex = result.lex;
    layout = result.layout;
    parse = result.parse;
  }
</script>

{#snippet performance(title: string, text: string)}
  <div class="performance">
    <span class="category">{title}</span>
    <span class="metric">{text}ms</span>
  </div>
{/snippet}

<div class="container">
  <div class="statistics">
    {@render performance("Lex", `${lex}`)}
    {@render performance("Layout", `${layout}`)}
    {@render performance("Parse", `${parse}`)}
  </div>
  <div class="grid">
    <div class="editor">
      <textarea bind:value={source} oninput={onSourceChange}></textarea>
    </div>
    <div class="preview">
      {#if output === ""}
        <pre>Welcome!</pre>
      {:else}
        <pre><code>{output}</code></pre>
      {/if}
    </div>
  </div>
</div>

<style>
  .container {
    display: flex;
    flex-direction: column;
    min-height: 0;
    height: 100%;
    gap: 1rem;
  }

  .statistics {
    font-family: var(--font-sans);
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(0, 1fr));
    grid-template-rows: minmax(0, 1fr);
    gap: 1rem;
  }

  .performance {
    background-color: oklch(var(--color-bg-lighter));
    display: flex;
    flex-direction: column;
    padding: 1rem;
    border-radius: 8px;
  }

  .category {
    font-size: 1rem;
    line-height: 1;
    font-weight: bold;
  }

  .metric {
    font-size: 1rem;
    flex-grow: 1;
    display: flex;
    align-items: end;
    justify-content: end;
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
    background-color: oklch(var(--color-bg-lighter));
    color: oklch(var(--color-fg));

    border: none;
    border-radius: 8px;

    box-sizing: border-box;
    resize: none;
    width: 100%;
    height: 100%;
    padding: 0.5rem;
  }

  .editor textarea:focus {
    outline: none;
  }

  .preview {
    overflow-y: auto;
    display: flex;
    flex-direction: column-reverse;
  }

  .preview > pre {
    flex-grow: 1;
  }
</style>
