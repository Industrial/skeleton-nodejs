import { Data, Effect, Either, Schema } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import { ExchangeIdSchema, validateExchangeId } from '../market-data/ExchangeId'
import { TimeframeSchema, validateTimeframe } from '../market-data/Timeframe'

// Error for invalid app config
export class InvalidAppConfigError extends Data.TaggedError(
  'InvalidAppConfigError',
)<{
  readonly message: string
  readonly field?: string
  readonly value?: string
}> {}

export const AppConfigSchema = Schema.Struct({
  exchange: ExchangeIdSchema,
  pair: Schema.String.pipe(
    Schema.nonEmptyString({
      message: () => 'pair should not be an empty string',
    }),
  ),
  timeframe: TimeframeSchema,
})

export type AppConfig = Schema.Schema.Type<typeof AppConfigSchema>

/**
 * Validates an AppConfig object
 */
export const validateAppConfig = (
  config: unknown,
): Effect.Effect<AppConfig, ParseError | InvalidAppConfigError, never> =>
  Effect.catchAll(
    Schema.decode(AppConfigSchema)(config as AppConfig),
    (error) =>
      Effect.fail(
        new InvalidAppConfigError({
          message: 'Invalid app configuration',
        }),
      ),
  )

/**
 * Creates an AppConfig with validation
 */
export const createAppConfig = (
  exchange: string,
  pair: string,
  timeframe: string,
): Effect.Effect<AppConfig, ParseError | InvalidAppConfigError, never> =>
  Effect.gen(function* (_) {
    // Validate individual fields
    if (!pair || pair.trim() === '') {
      return yield* Effect.fail(
        new InvalidAppConfigError({
          message: 'Pair cannot be empty',
          field: 'pair',
          value: pair,
        }),
      )
    }

    // Validate exchange and timeframe using their domain validators
    const validExchange = yield* Effect.catchAll(
      validateExchangeId(exchange),
      () =>
        Effect.fail(
          new InvalidAppConfigError({
            message: 'Invalid exchange ID',
            field: 'exchange',
            value: exchange,
          }),
        ),
    )

    const validTimeframe = yield* Effect.catchAll(
      validateTimeframe(timeframe),
      () =>
        Effect.fail(
          new InvalidAppConfigError({
            message: 'Invalid timeframe',
            field: 'timeframe',
            value: timeframe,
          }),
        ),
    )

    // Create and validate the complete config
    return yield* validateAppConfig({
      exchange: validExchange,
      pair,
      timeframe: validTimeframe,
    })
  })

/**
 * Checks if a configuration is valid
 */
export const isValidAppConfig = (
  config: unknown,
): Effect.Effect<boolean, never, never> =>
  Effect.gen(function* (_) {
    const result = yield* Effect.either(validateAppConfig(config))
    return Either.isRight(result)
  })
