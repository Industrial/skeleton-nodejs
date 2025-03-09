/**
 * @module Indicator
 * @description Domain model for technical indicators used in trading strategies.
 * Provides a generic interface for all indicators and factory functions to create them.
 */

import { Data, Effect, Schema } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'

/**
 * Error thrown when indicator parameters are invalid
 */
export class InvalidIndicatorParametersError extends Data.TaggedError(
  'InvalidIndicatorParametersError',
)<{
  readonly message: string
}> {}

/**
 * Enum for price types that can be used in indicator calculations
 */
export enum PriceType {
  Close = 'close',
  Open = 'open',
  High = 'high',
  Low = 'low',
  Typical = 'typical', // (high + low + close) / 3
  Median = 'median', // (high + low) / 2
  Volume = 'volume',
}

/**
 * Schema for price type selection
 */
export const PriceTypeSchema = Schema.Enums(PriceType)

/**
 * Base schema for indicator parameters
 */
export const BaseIndicatorParametersSchema = Schema.Struct({
  /** The number of periods to use in the calculation */
  period: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Period must be positive',
    }),
  ),
})

/**
 * Type for base indicator parameters
 */
export type BaseIndicatorParameters = Schema.Schema.Type<
  typeof BaseIndicatorParametersSchema
>

/**
 * Schema for indicator static properties
 * Note: We can't include the calculate function in the schema,
 * so we'll add it separately when creating the Indicator object
 */
export const IndicatorBaseSchema = Schema.Struct({
  /** Name of the indicator (e.g., "Simple Moving Average") */
  name: Schema.String,

  /** Description of what the indicator measures and how it's calculated */
  description: Schema.String,

  /** Configuration parameters for the indicator */
  parameters: Schema.Record({
    key: Schema.String,
    value: Schema.Unknown,
  }),
})

/**
 * Type derived from the schema, plus the calculate function
 * @template T The return type of the indicator calculation (default: number)
 */
export interface Indicator<T = number>
  extends Schema.Schema.Type<typeof IndicatorBaseSchema> {
  /**
   * Calculate the indicator value(s) from a series of candlesticks
   * @param candlesticks Array of candlesticks to calculate the indicator from
   * @returns Effect containing the calculated indicator value(s)
   */
  calculate: (candlesticks: Candlestick[]) => Effect.Effect<T, never, never>
}

/**
 * Get the price value from a candlestick based on the price type
 * @param candlestick The candlestick to extract the price from
 * @param priceType The type of price to extract
 * @returns The price value
 */
export const getPriceValue = (
  candlestick: Candlestick,
  priceType: PriceType = PriceType.Close,
): number => {
  switch (priceType) {
    case PriceType.Open:
      return candlestick.open
    case PriceType.High:
      return candlestick.high
    case PriceType.Low:
      return candlestick.low
    case PriceType.Close:
      return candlestick.close
    case PriceType.Typical:
      return (candlestick.high + candlestick.low + candlestick.close) / 3
    case PriceType.Median:
      return (candlestick.high + candlestick.low) / 2
    case PriceType.Volume:
      return candlestick.volume
    default:
      return candlestick.close
  }
}

/**
 * Creates an indicator with the specified parameters
 * @template T The return type of the indicator calculation
 * @param name Name of the indicator
 * @param description Description of the indicator
 * @param parameters Configuration parameters for the indicator
 * @param calculateFn Function to calculate the indicator value(s)
 * @returns Effect containing the created indicator
 * @throws {InvalidIndicatorParametersError} When parameters are invalid
 * @example
 * ```ts
 * const smaIndicator = createIndicator(
 *   'Simple Moving Average',
 *   'Average price over a specified number of periods',
 *   { period: 14, priceType: PriceType.Close },
 *   (candlesticks) => calculateSMA(candlesticks, 14)
 * )
 * ```
 */
export const createIndicator = <T>(
  name: string,
  description: string,
  parameters: Record<string, unknown>,
  calculateFn: (candlesticks: Candlestick[]) => Effect.Effect<T, never, never>,
): Effect.Effect<Indicator<T>, InvalidIndicatorParametersError, never> =>
  Effect.gen(function* (_) {
    // Validate name
    if (!name || name.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidIndicatorParametersError({
            message: 'Indicator name cannot be empty',
          }),
        ),
      )
    }

    // Validate parameters based on common requirements
    if (
      parameters.period !== undefined &&
      (typeof parameters.period !== 'number' || parameters.period <= 0)
    ) {
      return yield* _(
        Effect.fail(
          new InvalidIndicatorParametersError({
            message: 'Period must be a positive number',
          }),
        ),
      )
    }

    // Create the base indicator object
    const baseIndicator = {
      name,
      description,
      parameters,
    }

    // Validate using the schema
    try {
      const validatedBase =
        Schema.decodeSync(IndicatorBaseSchema)(baseIndicator)

      // Add the calculate function to create the complete Indicator
      const indicator: Indicator<T> = {
        ...validatedBase,
        calculate: calculateFn,
      }

      return indicator
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidIndicatorParametersError({
            message: `Invalid indicator parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Validates that there are enough candlesticks for the indicator calculation
 * @param candlesticks Array of candlesticks
 * @param minLength Minimum required length
 * @returns Effect containing the candlesticks or an error
 */
export const validateCandlestickLength = (
  candlesticks: Candlestick[],
  minLength: number,
): Effect.Effect<Candlestick[], InvalidIndicatorParametersError, never> =>
  candlesticks.length >= minLength
    ? Effect.succeed(candlesticks)
    : Effect.fail(
        new InvalidIndicatorParametersError({
          message: `Not enough data: need at least ${minLength} candlesticks, but got ${candlesticks.length}`,
        }),
      )

/**
 * Extract price values from candlesticks based on the specified price type
 * @param candlesticks Array of candlesticks
 * @param priceType Type of price to extract
 * @returns Array of price values
 */
export const extractPrices = (
  candlesticks: Candlestick[],
  priceType: PriceType = PriceType.Close,
): number[] => candlesticks.map((c) => getPriceValue(c, priceType))
