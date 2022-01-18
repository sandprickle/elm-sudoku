import { defineConfig } from 'windicss/helpers'

export default defineConfig({
  extract: {
    include: ['src/*.{html,elm,md}', 'index.html'],
    exclude: ['node_modules', '.git'],
  },
})
