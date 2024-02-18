// import { configDefaults, defineConfig } from 'vitest/config'
const { configDefaults, defineConfig } = require('vitest/config')

const exclude = [
  ...configDefaults.exclude,
  '**/.coverage/**',
  '**/.direnv/**',
  '**/.husky/**',
  '**/.venv/**',
  '**/.vscode/**',
  '**/node_modules/**',
]

export default defineConfig({
  test: {
    exclude,
    coverage: {
      exclude,
      reporter: ['text', 'json', 'html'],
      provider: 'v8',
    },
  },
  resolve: {
    // alias: {
    //   '@': new URL('./', import.meta.url).pathname,
    // },
  },
})
