import path from 'node:path'
import type { ViteUserConfig } from 'vitest/config'
import { defineConfig } from 'vitest/config'
import { baseVitestConfig } from '../../vitest.base.config'

const config = baseVitestConfig as ViteUserConfig
config.test = {
  ...config.test,
  coverage: {
    ...config.test?.coverage,
    reportsDirectory: path.resolve(
      __dirname,
      '../..',
      'dist',
      'coverage',
      '@susu/promise',
    ),
  },
}

export default defineConfig(config)
