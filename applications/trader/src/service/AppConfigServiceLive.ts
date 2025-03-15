import * as fs from 'node:fs'
import { Config, ConfigProvider, Effect, Schema } from 'effect'
import type { ConfigError } from 'effect/ConfigError'
import type { ParseError } from 'effect/ParseResult'
import type { AppConfig } from '../domain/config/AppConfig'
import { AppConfigSchema } from '../domain/config/AppConfig'
import type { ExchangeId } from '../domain/market-data/ExchangeId'
import type { Timeframe } from '../domain/market-data/Timeframe'
import { ConfigLoadError } from './AppConfigService'

/**
 * Live implementation of AppConfigService.
 * Loads configuration from JSON file, environment variables, and command-line arguments.
 */
export const loadConfig = (): Effect.Effect<
  AppConfig,
  ConfigLoadError | ParseError | ConfigError,
  never
> => {
  return Effect.gen(function* (_) {
    try {
      const jsonProvider = ConfigProvider.fromJson(
        JSON.parse(fs.readFileSync('config.json', 'utf-8')),
      )
      const envProvider = ConfigProvider.fromEnv()
      const configProvider = ConfigProvider.orElse(
        jsonProvider,
        () => envProvider,
      )

      const appConfig = yield* Effect.withConfigProvider(configProvider)(
        Effect.gen(function* (_) {
          const exchange = (yield* Config.string('exchange')) as ExchangeId
          const pair = yield* Config.string('pair')
          const timeframe = (yield* Config.string('timeframe')) as Timeframe

          const decodedSchema = yield* Schema.decode(AppConfigSchema)({
            exchange,
            pair,
            timeframe,
          })

          return decodedSchema
        }),
      )

      return appConfig
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
