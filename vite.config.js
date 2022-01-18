import { defineConfig } from 'vite'
// import WindiCSS from 'vite-plugin-windicss'
import elmPlugin from 'vite-plugin-elm'

export default defineConfig({
  plugins: [elmPlugin()],
})
