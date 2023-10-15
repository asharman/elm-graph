import { defineConfig } from "vite";
import plugin from "vite-plugin-elm";

export default defineConfig({
  base: "/elm-graph/",
  plugins: [plugin()],
  build: { outDir: "./dist" },
});
