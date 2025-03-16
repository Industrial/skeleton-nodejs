import { Context, Data, type Effect, Layer } from 'effect'
import type { ConfigError } from 'effect/ConfigError'
import type { ParseError } from 'effect/ParseResult'
import type { AppConfig } from '../domain/config/AppConfig'
import * as AppConfigServiceLive from './AppConfigServiceLive'

export class ConfigLoadError extends Data.TaggedError('ConfigLoadError')<{
  readonly message: string
  readonly cause?: unknown
}> {}

export interface AppConfigServiceType {
  /**
   * Loads application configuration from various sources (JSON, ENV vars, CLI args)
   * @returns Effect containing the validated AppConfig domain model
   */
  loadConfig(): Effect.Effect<
    AppConfig,
    ConfigLoadError | ConfigError | ParseError,
    never
  >
}

export class AppConfigService extends Context.Tag('AppConfigService')<
  AppConfigServiceType,
  AppConfigServiceType
>() {
  static Live = Layer.succeed(this, AppConfigServiceLive)
}
