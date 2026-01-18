import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import Icons from "unplugin-icons/vite";

export default defineConfig({
  base: "/purescript-analyzer/",
  plugins: [react(), tailwindcss(), Icons({ compiler: "jsx", jsx: "react" })],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
    },
  },
  build: {
    outDir: "build",
    target: "esnext",
  },
  worker: {
    format: "es",
  },
  optimizeDeps: {
    exclude: ["docs-lib"],
  },
});
