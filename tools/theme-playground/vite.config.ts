import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "path";

export default defineConfig({
  root: "public",
  plugins: [react()],
  server: {
    port: 5173,
    proxy: { "/api": "http://localhost:5174" },
  },
  resolve: {
    alias: { "@": new URL("./src", import.meta.url).pathname },
  },
  build: {
    rollupOptions: {
      input: path.resolve(__dirname, "public/index.html"),
    },
  },
});
