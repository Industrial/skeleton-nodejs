/**
 * @module Strategy
 * @description Domain model for trading strategies.
 * Provides interfaces and factory functions for creating and executing trading strategies.
 */

import { Data, Effect, Schema } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'
import type { Indicator } from './Indicator'
import type { Signal } from './Signal'
import { InvalidStrategyParametersError } from './StrategyParameters'

/**
 * Error thrown when strategy execution fails
 */
export class StrategyExecutionError extends Data.TaggedError(
  'StrategyExecutionError',
)<{
  readonly message: string
  readonly cause?: unknown
}> {}

/**
 * Schema for strategy
 * Note: We can't directly include the analyze function in the schema,
 * so we'll add it separately when creating the Strategy object
 */
export const StrategyBaseSchema = Schema.Struct({
  /** Name of the strategy */
  name: Schema.String,

  /** Description of what the strategy does */
  description: Schema.String,

  /** Configuration parameters for the strategy */
  parameters: Schema.Record({ key: Schema.String, value: Schema.Unknown }),

  /** Indicators used by the strategy */
  indicators: Schema.Array(Schema.Unknown),
})

/**
 * Type derived from the schema, plus the analyze function
 */
export interface Strategy
  extends Schema.Schema.Type<typeof StrategyBaseSchema> {
  /**
   * Analyze candlesticks and generate trading signals
   * @param candlesticks Array of candlesticks to analyze
   * @returns Effect containing array of signals
   */
  analyze: (
    candlesticks: Candlestick[],
  ) => Effect.Effect<Signal[], StrategyExecutionError, never>
}

/**
 * Creates a strategy with the specified parameters
 * @param name Name of the strategy
 * @param description Description of the strategy
 * @param parameters Configuration parameters
 * @param indicators Indicators used by the strategy
 * @param analyzeFn Function to analyze candlesticks and generate signals
 * @returns Effect containing the created strategy
 * @throws {InvalidStrategyParametersError} When parameters are invalid
 */
export const createStrategy = (
  name: string,
  description: string,
  parameters: Record<string, unknown>,
  indicators: Indicator[],
  analyzeFn: (
    candlesticks: Candlestick[],
  ) => Effect.Effect<Signal[], StrategyExecutionError, never>,
): Effect.Effect<Strategy, InvalidStrategyParametersError, never> =>
  Effect.gen(function* (_) {
    // Validate name
    if (!name || name.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Strategy name cannot be empty',
          }),
        ),
      )
    }

    // Validate description
    if (!description || description.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidStrategyParametersError({
            message: 'Strategy description cannot be empty',
          }),
        ),
      )
    }

    // Create the base strategy object
    const baseStrategy = {
      name,
      description,
      parameters,
      indicators,
    }

    // Validate using the schema
    try {
      const validatedBase = Schema.decodeSync(StrategyBaseSchema)(baseStrategy)

      // Add the analyze function to create the complete Strategy
      const strategy: Strategy = {
        ...validatedBase,
        analyze: analyzeFn,
      }

      return strategy
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
