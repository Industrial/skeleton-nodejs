import * as fs from 'node:fs'
import { Config, ConfigProvider, Effect, Schema } from 'effect'
import type { ConfigError } from 'effect/ConfigError'
import type { ParseError } from 'effect/ParseResult'
import {
  type AppConfig,
  type InvalidAppConfigError,
  createAppConfig,
} from '../domain/config/AppConfig'
import { ConfigLoadError } from './AppConfigService'

/**
 * Live implementation of AppConfigService.
 * Loads configuration from JSON file, environment variables, and command-line arguments.
 */
export const loadConfig = (): Effect.Effect<
  AppConfig,
  ConfigLoadError | ParseError | ConfigError | InvalidAppConfigError,
  never
> => {
  return Effect.gen(function* (_) {
    try {
      // Load raw config data from sources
      const jsonProvider = ConfigProvider.fromJson(
        JSON.parse(fs.readFileSync('config.json', 'utf-8')),
      )
      const envProvider = ConfigProvider.fromEnv()
      const configProvider = ConfigProvider.orElse(
        jsonProvider,
        () => envProvider,
      )

      // Extract raw values
      const rawConfig = yield* Effect.withConfigProvider(configProvider)(
        Effect.gen(function* (_) {
          const exchange = yield* Config.string('exchange')
          const pair = yield* Config.string('pair')
          const timeframe = yield* Config.string('timeframe')

          return { exchange, pair, timeframe }
        }),
      )

      // Use domain model function to create and validate AppConfig
      return yield* createAppConfig(
        rawConfig.exchange,
        rawConfig.pair,
        rawConfig.timeframe,
      )
    } catch (error) {
      return yield* Effect.fail(
        new ConfigLoadError({
          message: 'Failed to load config',
          cause: error,
        }),
      )
    }
  })
}
