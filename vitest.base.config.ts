import { configDefaults, UserConfig } from 'vitest/config'

const exclude = [
  ...configDefaults.exclude,
  '**/.coverage/**',
  '**/.direnv/**',
  '**/.husky/**',
  '**/.venv/**',
  '**/.vscode/**',
  '**/node_modules/**',
  './.eslintignore',
  './.eslintrc.json',
  './.prettierignore',
  './.prettierrc.json',
  './.swcrc',
  './.tsbuildinfo',
  './package.json',
  './project.json',
  './tsconfig.json',
  './vitest.config.ts',
]

export const baseVitestConfig: UserConfig = {
  test: {
    passWithNoTests: true,
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
}
