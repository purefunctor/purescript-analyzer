<script lang="ts">
  import "@fontsource-variable/montserrat";

  let { children } = $props();
</script>

{@render children()}

<style>
  :root {
    --color-bg: 22.64% 0 0;
    --color-bg-lighter: 26.64% 0 0;
    --color-fg: 98.65% 0.0066 286.28;
    --font-sans: "Montserrat Variable", sans-serif;
  }

  :global body {
    background-color: oklch(var(--color-bg));
    color: oklch(var(--color-fg));
  }
</style>
