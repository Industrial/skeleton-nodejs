/**
 * @module StrategyParameters
 * @description Domain model for strategy parameters used to configure trading strategies.
 * Provides validation and type-safe handling of strategy configuration.
 */

import { Data, Effect, Schema } from 'effect'
import { PriceType } from './Indicator'

/**
 * Error thrown when strategy parameters are invalid
 */
export class InvalidStrategyParametersError extends Data.TaggedError(
  'InvalidStrategyParametersError',
)<{
  readonly message: string
}> {}

/**
 * Base schema for strategy parameters that all strategies should include
 */
export const BaseStrategyParametersSchema = Schema.Struct({
  /** Unique name of the strategy */
  name: Schema.String,

  /** Description of what the strategy does and how it works */
  description: Schema.String.pipe(Schema.optional),

  /** Optional metadata for additional strategy information */
  metadata: Schema.Record({
    key: Schema.String,
    value: Schema.Unknown,
  }).pipe(Schema.optional),
})

/**
 * Type for base strategy parameters
 */
export type BaseStrategyParameters = Schema.Schema.Type<
  typeof BaseStrategyParametersSchema
>

/**
 * Schema for moving average crossover strategy parameters
 */
export const MovingAverageCrossoverParametersSchema = Schema.Struct({
  ...BaseStrategyParametersSchema.fields,

  /** Period for the fast moving average */
  fastPeriod: Schema.Number,

  /** Period for the slow moving average */
  slowPeriod: Schema.Number,

  /** Price type to use for calculations */
  priceType: Schema.Enums(PriceType).pipe(Schema.optional),
})

/**
 * Type for moving average crossover strategy parameters
 */
export type MovingAverageCrossoverParameters = Schema.Schema.Type<
  typeof MovingAverageCrossoverParametersSchema
>

/**
 * Schema for RSI strategy parameters
 */
export const RSIStrategyParametersSchema = Schema.Struct({
  ...BaseStrategyParametersSchema.fields,

  /** Period for RSI calculation */
  period: Schema.Number,

  /** Overbought threshold (typically 70) */
  overbought: Schema.Number,

  /** Oversold threshold (typically 30) */
  oversold: Schema.Number,

  /** Price type to use for calculations */
  priceType: Schema.Enums(PriceType).pipe(Schema.optional),
})

/**
 * Type for RSI strategy parameters
 */
export type RSIStrategyParameters = Schema.Schema.Type<
  typeof RSIStrategyParametersSchema
>

/**
 * Creates validated base strategy parameters
 * @param params Base strategy parameters
 * @returns Effect containing validated parameters
 * @throws {InvalidStrategyParametersError} When parameters are invalid
 */
export const createBaseStrategyParameters = (params: {
  name: string
  description?: string
  metadata?: Record<string, unknown>
}): Effect.Effect<
  BaseStrategyParameters,
  InvalidStrategyParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate name
    if (!params.name || params.name.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Strategy name cannot be empty',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(BaseStrategyParametersSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: `Invalid strategy parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates validated moving average crossover strategy parameters
 * @param params Moving average crossover strategy parameters
 * @returns Effect containing validated parameters
 * @throws {InvalidStrategyParametersError} When parameters are invalid
 * @example
 * ```ts
 * const params = createMovingAverageCrossoverParameters({
 *   name: 'MA Crossover',
 *   description: 'Simple moving average crossover strategy',
 *   fastPeriod: 10,
 *   slowPeriod: 20,
 *   priceType: PriceType.Close
 * })
 * ```
 */
export const createMovingAverageCrossoverParameters = (params: {
  name: string
  description?: string
  fastPeriod: number
  slowPeriod: number
  priceType?: PriceType
  metadata?: Record<string, unknown>
}): Effect.Effect<
  MovingAverageCrossoverParameters,
  InvalidStrategyParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate name
    if (!params.name || params.name.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Strategy name cannot be empty',
          }),
        ),
      )
    }

    // Validate periods
    if (params.fastPeriod <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Fast period must be positive',
          }),
        ),
      )
    }

    if (params.slowPeriod <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Slow period must be positive',
          }),
        ),
      )
    }

    if (params.fastPeriod >= params.slowPeriod) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Fast period must be less than slow period',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(MovingAverageCrossoverParametersSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: `Invalid strategy parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates validated RSI strategy parameters
 * @param params RSI strategy parameters
 * @returns Effect containing validated parameters
 * @throws {InvalidStrategyParametersError} When parameters are invalid
 * @example
 * ```ts
 * const params = createRSIStrategyParameters({
 *   name: 'RSI Strategy',
 *   description: 'Relative Strength Index strategy',
 *   period: 14,
 *   overbought: 70,
 *   oversold: 30,
 *   priceType: PriceType.Close
 * })
 * ```
 */
export const createRSIStrategyParameters = (params: {
  name: string
  description?: string
  period: number
  overbought: number
  oversold: number
  priceType?: PriceType
  metadata?: Record<string, unknown>
}): Effect.Effect<
  RSIStrategyParameters,
  InvalidStrategyParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate name
    if (!params.name || params.name.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Strategy name cannot be empty',
          }),
        ),
      )
    }

    // Validate period
    if (params.period <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Period must be positive',
          }),
        ),
      )
    }

    // Validate thresholds
    if (params.overbought <= params.oversold) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message:
              'Overbought threshold must be greater than oversold threshold',
          }),
        ),
      )
    }

    if (params.overbought < 0 || params.overbought > 100) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Overbought threshold must be between 0 and 100',
          }),
        ),
      )
    }

    if (params.oversold < 0 || params.oversold > 100) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Oversold threshold must be between 0 and 100',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(RSIStrategyParametersSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: `Invalid strategy parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })
