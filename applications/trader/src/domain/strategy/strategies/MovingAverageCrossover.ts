/**
 * @module MovingAverageCrossover
 * @description Implementation of Moving Average Crossover strategy.
 * Generates buy signals when fast MA crosses above slow MA, and sell signals when fast MA crosses below slow MA.
 */

import { Effect, Schema } from 'effect'
import type { Candlestick } from '../../market-data/Candlestick'
import {
  type Indicator,
  type InvalidIndicatorParametersError,
  PriceType,
} from '../Indicator'
import {
  type Signal,
  createBuySignal,
  createNoSignal,
  createSellSignal,
} from '../Signal'
import {
  type Strategy,
  StrategyExecutionError,
  createStrategy,
} from '../Strategy'
import {
  InvalidStrategyParametersError,
  MovingAverageCrossoverParametersSchema,
} from '../StrategyParameters'
import {
  createExponentialMovingAverage,
  createMovingAverage,
} from '../indicators/MovingAverage'

/**
 * Type of moving average to use
 */
export enum MovingAverageType {
  Simple = 'simple',
  Exponential = 'exponential',
}

/**
 * Extended schema for moving average crossover strategy parameters
 */
export const ExtendedMovingAverageCrossoverParametersSchema = Schema.Struct({
  ...MovingAverageCrossoverParametersSchema.fields,

  /** Type of moving average to use (default: simple) */
  maType: Schema.Enums(MovingAverageType).pipe(Schema.optional),
})

/**
 * Type for extended moving average crossover parameters
 */
export type ExtendedMovingAverageCrossoverParameters = Schema.Schema.Type<
  typeof ExtendedMovingAverageCrossoverParametersSchema
>

/**
 * Creates a Moving Average Crossover strategy
 * @param params Strategy parameters
 * @returns Effect containing the created strategy
 * @throws {InvalidStrategyParametersError} When strategy parameters are invalid
 * @throws {InvalidIndicatorParametersError} When indicator parameters are invalid
 * @example
 * ```ts
 * const strategy = createMovingAverageCrossoverStrategy({
 *   name: 'MA Crossover',
 *   description: 'Simple moving average crossover strategy',
 *   fastPeriod: 10,
 *   slowPeriod: 20,
 *   priceType: PriceType.Close,
 *   maType: MovingAverageType.Simple
 * })
 * ```
 */
export const createMovingAverageCrossoverStrategy = (params: {
  name: string
  description?: string
  fastPeriod: number
  slowPeriod: number
  priceType?: PriceType
  maType?: MovingAverageType
  metadata?: Record<string, unknown>
}): Effect.Effect<
  Strategy,
  InvalidStrategyParametersError | InvalidIndicatorParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate parameters
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

    // Create moving average indicators based on the type
    const maType = params.maType || MovingAverageType.Simple
    const createMA =
      maType === MovingAverageType.Simple
        ? createMovingAverage
        : createExponentialMovingAverage

    // Create fast moving average indicator
    const fastMA = yield* _(
      createMA({
        period: params.fastPeriod,
        priceType: params.priceType,
      }),
    )

    // Create slow moving average indicator
    const slowMA = yield* _(
      createMA({
        period: params.slowPeriod,
        priceType: params.priceType,
      }),
    )

    // Create the strategy analyze function
    const analyzeFn = (
      candlesticks: Candlestick[],
    ): Effect.Effect<Signal[], StrategyExecutionError, never> =>
      Effect.gen(function* ($) {
        try {
          // Calculate indicators
          const fastValues = yield* $(
            fastMA.calculate(candlesticks).pipe(
              Effect.mapError(
                (error) =>
                  new StrategyExecutionError({
                    message: `Error calculating fast MA: ${String(error)}`,
                    cause: error,
                  }),
              ),
            ),
          )

          const slowValues = yield* $(
            slowMA.calculate(candlesticks).pipe(
              Effect.mapError(
                (error) =>
                  new StrategyExecutionError({
                    message: `Error calculating slow MA: ${String(error)}`,
                    cause: error,
                  }),
              ),
            ),
          )

          // Generate signals
          const signals: Signal[] = []

          // We need at least 2 values to detect a crossover
          if (fastValues.length < 2 || slowValues.length < 2) {
            return signals
          }

          // Start from the second value to compare with previous
          for (
            let i = 1;
            i < Math.min(fastValues.length, slowValues.length);
            i++
          ) {
            const timestamp = candlesticks[i].timestamp
            const price = candlesticks[i].close
            const fastPrev = fastValues[i - 1]
            const fastCurr = fastValues[i]
            const slowPrev = slowValues[i - 1]
            const slowCurr = slowValues[i]

            // Detect crossover: fast MA crosses above slow MA (buy signal)
            if (fastPrev < slowPrev && fastCurr > slowCurr) {
              const signal = yield* $(
                createBuySignal(price, timestamp, {
                  metadata: {
                    fastMA: fastCurr,
                    slowMA: slowCurr,
                    crossover: 'above',
                  },
                }).pipe(
                  Effect.mapError(
                    (error) =>
                      new StrategyExecutionError({
                        message: `Error creating buy signal: ${String(error)}`,
                        cause: error,
                      }),
                  ),
                ),
              )
              signals.push(signal)
            }
            // Detect crossover: fast MA crosses below slow MA (sell signal)
            else if (fastPrev > slowPrev && fastCurr < slowCurr) {
              const signal = yield* $(
                createSellSignal(price, timestamp, {
                  metadata: {
                    fastMA: fastCurr,
                    slowMA: slowCurr,
                    crossover: 'below',
                  },
                }).pipe(
                  Effect.mapError(
                    (error) =>
                      new StrategyExecutionError({
                        message: `Error creating sell signal: ${String(error)}`,
                        cause: error,
                      }),
                  ),
                ),
              )
              signals.push(signal)
            }
            // No crossover (no signal)
            else {
              const signal = yield* $(
                createNoSignal(price, timestamp, {
                  metadata: {
                    fastMA: fastCurr,
                    slowMA: slowCurr,
                  },
                }).pipe(
                  Effect.mapError(
                    (error) =>
                      new StrategyExecutionError({
                        message: `Error creating no signal: ${String(error)}`,
                        cause: error,
                      }),
                  ),
                ),
              )
              signals.push(signal)
            }
          }

          return signals
        } catch (error) {
          return yield* $(
            Effect.fail(
              new StrategyExecutionError({
                message: `Unexpected error in strategy execution: ${String(
                  error,
                )}`,
                cause: error,
              }),
            ),
          )
        }
      })

    // Create the strategy
    return yield* _(
      createStrategy(
        params.name,
        params.description || 'Moving Average Crossover Strategy',
        {
          fastPeriod: params.fastPeriod,
          slowPeriod: params.slowPeriod,
          priceType: params.priceType || PriceType.Close,
          maType: params.maType || MovingAverageType.Simple,
          ...params.metadata,
        },
        // Use type assertion to handle the Indicator<number[]> vs Indicator<number> issue
        // This is safe because the Strategy implementation can handle different indicator types
        [fastMA, slowMA] as unknown as Indicator[],
        analyzeFn,
      ),
    )
  })
