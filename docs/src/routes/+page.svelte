<script lang="ts">
  import { createDocsLib } from "$lib";
  import Editor from "./editor.svelte";

  let docsLib = $state(createDocsLib());
</script>

<div class="container">
  <h1>PureScript Analyzer Lossless CST</h1>
  {#await docsLib}
    <div class="placeholder">Loading...</div>
  {:then docsLib}
    <Editor {docsLib} />
  {:catch error}
    <div class="placeholder">{error}</div>
  {/await}
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
    gap: 1rem;
  }

  .container h1 {
    font-family: var(--font-sans);
    margin: 0;
  }

  .placeholder {
    font-family: var(--font-sans);
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100%;
  }
</style>
