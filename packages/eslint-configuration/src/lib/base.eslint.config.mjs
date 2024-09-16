import { fixupConfigRules } from '@eslint/compat'
import { FlatCompat } from '@eslint/eslintrc'
import typescriptPlugin from '@typescript-eslint/eslint-plugin'
import typescriptParser from '@typescript-eslint/parser'
import jsxA11yPlugin from 'eslint-plugin-jsx-a11y'
import prettierPlugin from 'eslint-plugin-prettier'
import reactHooksPlugin from 'eslint-plugin-react-hooks'
import simpleImportSortPlugin from 'eslint-plugin-simple-import-sort'
import sonarjsPlugin from 'eslint-plugin-sonarjs'
import { dirname } from 'path'
import { fileURLToPath } from 'url'

// This exports an object so that it can be used in the eslint.config.js array.
export default [
  ...fixupConfigRules(
    new FlatCompat({
      baseDirectory: dirname(fileURLToPath(import.meta.url)),
    }).extends('plugin:@next/next/core-web-vitals', 'prettier'),
  ),
  {
    files: ['**/*.{js,mjs,cjs,jsx,ts,tsx}'],
    plugins: {
      '@typescript-eslint': typescriptPlugin,
      'jsx-a11y': jsxA11yPlugin,
      'react-hooks': reactHooksPlugin,
      'simple-import-sort': simpleImportSortPlugin,
      'prettier': prettierPlugin,
      'sonarjs': sonarjsPlugin,
    },
    ignores: [
      '**/.direnv',
      '**/.envrc',
      '**/.eslintcache',
      '**/.eslintrc.json',
      '**/.github',
      '**/.gitignore',
      '**/.husky',
      '**/.nx',
      '**/.pre-commit-config.yaml',
      '**/.prettierignore',
      '**/.prettierignore',
      '**/.tsbuildinfo',
      '**/.vercel',
      '**/.vscode',
      '**/bun.lockb',
      '**/coverage',
      '**/flake.lock',
      '**/flake.nix',
      '**/node_modules',
      '**/prompts',
    ],
    languageOptions: {
      parser: typescriptParser,
    },
    rules: {
      // Recommended
      'constructor-super': 'error',
      'for-direction': 'error',
      'getter-return': 'error',
      'no-async-promise-executor': 'error',
      'no-case-declarations': 'error',
      'no-class-assign': 'error',
      'no-compare-neg-zero': 'error',
      'no-cond-assign': 'error',
      'no-const-assign': 'error',
      'no-constant-binary-expression': 'error',
      'no-constant-condition': 'error',
      'no-control-regex': 'error',
      'no-debugger': 'error',
      'no-delete-var': 'error',
      'no-dupe-args': 'error',
      'no-dupe-class-members': 'error',
      'no-dupe-else-if': 'error',
      'no-dupe-keys': 'error',
      'no-duplicate-case': 'error',
      'no-empty': 'error',
      'no-empty-character-class': 'error',
      'no-empty-pattern': 'error',
      'no-empty-static-block': 'error',
      'no-ex-assign': 'error',
      'no-extra-boolean-cast': 'error',
      'no-fallthrough': 'error',
      'no-func-assign': 'error',
      'no-global-assign': 'error',
      'no-import-assign': 'error',
      'no-invalid-regexp': 'error',
      'no-irregular-whitespace': 'error',
      'no-loss-of-precision': 'error',
      'no-misleading-character-class': 'error',
      'no-new-native-nonconstructor': 'error',
      'no-nonoctal-decimal-escape': 'error',
      'no-obj-calls': 'error',
      'no-octal': 'error',
      'no-prototype-builtins': 'error',
      'no-redeclare': 'error',
      'no-regex-spaces': 'error',
      'no-self-assign': 'error',
      'no-setter-return': 'error',
      'no-shadow-restricted-names': 'error',
      'no-sparse-arrays': 'error',
      'no-this-before-super': 'error',
      'no-unexpected-multiline': 'error',
      'no-unreachable': 'error',
      'no-unsafe-finally': 'error',
      'no-unsafe-negation': 'error',
      'no-unsafe-optional-chaining': 'error',
      'no-unused-labels': 'error',
      'no-unused-private-class-members': 'error',
      'no-useless-backreference': 'error',
      'no-useless-catch': 'error',
      'no-useless-escape': 'error',
      'no-with': 'error',
      'require-yield': 'error',
      'use-isnan': 'error',
      'valid-typeof': 'error',

      // headless-commerce
      'no-console': 'error',
      'curly': 'error',
      'no-undef': 'off',
      'no-unused-vars': 'off',
      // "@typescript-eslint/no-explicit-any": "error",
      '@typescript-eslint/no-unused-vars': 'error',
      '@typescript-eslint/consistent-type-imports': [
        'error',
        {
          prefer: 'type-imports',
          fixStyle: 'separate-type-imports',
        },
      ],
      'prettier/prettier': [
        'error',
        {
          endOfLine: 'auto',
        },
      ],
      'simple-import-sort/exports': ['error'],
      'simple-import-sort/imports': ['error'],
    },
  },
]
