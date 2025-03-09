/**
 * @module MovingAverage
 * @description Implementation of Moving Average indicators.
 * Provides functions to calculate Simple Moving Average (SMA) and create MA indicators.
 */

import { Effect, Schema } from 'effect'
import type { Candlestick } from '../../market-data/Candlestick'
import {
  BaseIndicatorParametersSchema,
  type Indicator,
  InvalidIndicatorParametersError,
  PriceType,
  PriceTypeSchema,
  createIndicator,
  extractPrices,
  validateCandlestickLength,
} from '../Indicator'

/**
 * Schema for moving average parameters
 */
export const MovingAverageParametersSchema = Schema.Struct({
  ...BaseIndicatorParametersSchema.fields,

  /** Price type to use for calculations (default: close) */
  priceType: PriceTypeSchema.pipe(Schema.optional),
})

/**
 * Type for moving average parameters
 */
export type MovingAverageParameters = Schema.Schema.Type<
  typeof MovingAverageParametersSchema
>

/**
 * Calculate Simple Moving Average (SMA) from an array of values
 * @param values Array of numeric values
 * @param period Number of periods to average
 * @returns Array of SMA values
 */
export const calculateSMA = (values: number[], period: number): number[] => {
  if (values.length < period) {
    return []
  }

  const result: number[] = []
  let sum = 0

  // Calculate first sum
  for (let i = 0; i < period; i++) {
    sum += values[i]
  }
  result.push(sum / period)

  // Calculate remaining sums using sliding window
  for (let i = period; i < values.length; i++) {
    sum = sum - values[i - period] + values[i]
    result.push(sum / period)
  }

  return result
}

/**
 * Calculate Simple Moving Average (SMA) from candlesticks
 * @param candlesticks Array of candlesticks
 * @param period Number of periods to average
 * @param priceType Price type to use (default: close)
 * @returns Effect containing array of SMA values
 */
export const calculateSMAFromCandlesticks = (
  candlesticks: Candlestick[],
  period: number,
  priceType: PriceType = PriceType.Close,
): Effect.Effect<number[], never, never> => {
  // Extract prices and calculate SMA
  const prices = extractPrices(candlesticks, priceType)
  return Effect.succeed(calculateSMA(prices, period))
}

/**
 * Creates a Simple Moving Average (SMA) indicator
 * @param params Moving average parameters
 * @returns Effect containing the created indicator
 * @throws {InvalidIndicatorParametersError} When parameters are invalid
 * @example
 * ```ts
 * const sma = createMovingAverage({
 *   period: 14,
 *   priceType: PriceType.Close
 * })
 * ```
 */
export const createMovingAverage = (params: {
  period: number
  priceType?: PriceType
}): Effect.Effect<
  Indicator<number[]>,
  InvalidIndicatorParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate period
    if (params.period <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidIndicatorParametersError({
            message: 'Period must be positive',
          }),
        ),
      )
    }

    // Create a calculate function that handles validation internally
    const calculateFn = (
      candlesticks: Candlestick[],
    ): Effect.Effect<number[], never, never> => {
      // Validate candlestick length
      if (candlesticks.length < params.period) {
        return Effect.succeed([])
      }

      // Calculate SMA
      return Effect.succeed(
        calculateSMA(
          extractPrices(candlesticks, params.priceType || PriceType.Close),
          params.period,
        ),
      )
    }

    // Create the indicator
    return yield* _(
      createIndicator(
        `Simple Moving Average (${params.period})`,
        `Average price over ${params.period} periods`,
        {
          period: params.period,
          priceType: params.priceType || PriceType.Close,
        },
        calculateFn,
      ),
    )
  })

/**
 * Calculate Exponential Moving Average (EMA) from an array of values
 * @param values Array of numeric values
 * @param period Number of periods for EMA
 * @returns Array of EMA values
 */
export const calculateEMA = (values: number[], period: number): number[] => {
  if (values.length < period) {
    return []
  }

  const result: number[] = []
  const multiplier = 2 / (period + 1)

  // Start with SMA for the first value
  const firstSMA =
    values.slice(0, period).reduce((sum, val) => sum + val, 0) / period
  result.push(firstSMA)

  // Calculate EMA for remaining values
  for (let i = period; i < values.length; i++) {
    const ema =
      (values[i] - result[result.length - 1]) * multiplier +
      result[result.length - 1]
    result.push(ema)
  }

  return result
}

/**
 * Calculate Exponential Moving Average (EMA) from candlesticks
 * @param candlesticks Array of candlesticks
 * @param period Number of periods for EMA
 * @param priceType Price type to use (default: close)
 * @returns Effect containing array of EMA values
 */
export const calculateEMAFromCandlesticks = (
  candlesticks: Candlestick[],
  period: number,
  priceType: PriceType = PriceType.Close,
): Effect.Effect<number[], never, never> => {
  // Extract prices and calculate EMA
  const prices = extractPrices(candlesticks, priceType)
  return Effect.succeed(calculateEMA(prices, period))
}

/**
 * Creates an Exponential Moving Average (EMA) indicator
 * @param params Moving average parameters
 * @returns Effect containing the created indicator
 * @throws {InvalidIndicatorParametersError} When parameters are invalid
 * @example
 * ```ts
 * const ema = createExponentialMovingAverage({
 *   period: 14,
 *   priceType: PriceType.Close
 * })
 * ```
 */
export const createExponentialMovingAverage = (params: {
  period: number
  priceType?: PriceType
}): Effect.Effect<
  Indicator<number[]>,
  InvalidIndicatorParametersError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate period
    if (params.period <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidIndicatorParametersError({
            message: 'Period must be positive',
          }),
        ),
      )
    }

    // Create a calculate function that handles validation internally
    const calculateFn = (
      candlesticks: Candlestick[],
    ): Effect.Effect<number[], never, never> => {
      // Validate candlestick length
      if (candlesticks.length < params.period) {
        return Effect.succeed([])
      }

      // Calculate EMA
      return Effect.succeed(
        calculateEMA(
          extractPrices(candlesticks, params.priceType || PriceType.Close),
          params.period,
        ),
      )
    }

    // Create the indicator
    return yield* _(
      createIndicator(
        `Exponential Moving Average (${params.period})`,
        `Exponentially weighted average price over ${params.period} periods`,
        {
          period: params.period,
          priceType: params.priceType || PriceType.Close,
        },
        calculateFn,
      ),
    )
  })
