import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  root: "public",
  server: {
    port: 5173,
    proxy: { "/api": "http://localhost:5174" },
  },
  resolve: {
    alias: { "@": new URL("./src", import.meta.url).pathname },
  },
});
