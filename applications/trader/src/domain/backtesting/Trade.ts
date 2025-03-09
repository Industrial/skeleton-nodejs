/**
 * @module Trade
 * @description Domain model for trades in backtesting.
 * Represents a single buy or sell action with price, volume, and timestamp.
 */

import { Data, Effect, Schema } from 'effect'

/**
 * Enum for trade directions
 */
export enum TradeDirection {
  /** Buy trade (long) */
  Buy = 'buy',

  /** Sell trade (short or exit) */
  Sell = 'sell',
}

/**
 * Error thrown when trade parameters are invalid
 */
export class InvalidTradeError extends Data.TaggedError('InvalidTradeError')<{
  readonly message: string
}> {}

/**
 * Schema for trade data
 */
export const TradeSchema = Schema.Struct({
  /** Direction of the trade (buy or sell) */
  direction: Schema.Enums(TradeDirection),

  /** Price at which the trade was executed */
  price: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Price must be positive',
    }),
  ),

  /** Volume of the trade (quantity) */
  volume: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Volume must be positive',
    }),
  ),

  /** Timestamp when the trade was executed (Unix timestamp in milliseconds) */
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),

  /** Trading fees for this trade (as a decimal, e.g., 0.001 for 0.1%) */
  fees: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Fees must be non-negative',
    }),
  ),

  /** Optional identifier for the trade */
  id: Schema.optional(Schema.String),

  /** Optional metadata for additional trade information */
  metadata: Schema.optional(
    Schema.Record({ key: Schema.String, value: Schema.Unknown }),
  ),
})

/**
 * Type for validated trade
 */
export type Trade = Schema.Schema.Type<typeof TradeSchema>

/**
 * Interface for trade parameters
 */
export interface TradeParams {
  /** Direction of the trade (buy or sell) */
  direction: TradeDirection
  /** Price at which the trade was executed */
  price: number
  /** Volume of the trade (quantity) */
  volume: number
  /** Timestamp when the trade was executed */
  timestamp: number
  /** Trading fees for this trade */
  fees: number
  /** Optional identifier for the trade */
  id?: string
  /** Optional metadata for additional trade information */
  metadata?: Record<string, unknown>
}

/**
 * Creates a validated trade
 * @param params Trade parameters
 * @returns Effect containing the validated trade
 * @throws {InvalidTradeError} When trade parameters are invalid
 * @example
 * ```ts
 * const trade = createTrade({
 *   direction: TradeDirection.Buy,
 *   price: 50000,
 *   volume: 0.1,
 *   timestamp: Date.now(),
 *   fees: 0.001
 * })
 * ```
 */
export const createTrade = (
  params: TradeParams,
): Effect.Effect<Trade, InvalidTradeError, never> =>
  Effect.gen(function* (_) {
    // Validate price
    if (params.price <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTradeError({
            message: 'Price must be positive',
          }),
        ),
      )
    }

    // Validate volume
    if (params.volume <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTradeError({
            message: 'Volume must be positive',
          }),
        ),
      )
    }

    // Validate timestamp
    if (params.timestamp <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTradeError({
            message: 'Timestamp must be positive',
          }),
        ),
      )
    }

    // Validate fees
    if (params.fees < 0) {
      return yield* _(
        Effect.fail(
          new InvalidTradeError({
            message: 'Fees must be non-negative',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(TradeSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidTradeError({
            message: `Invalid trade parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates a buy trade
 * @param price Price at which to buy
 * @param volume Volume to buy
 * @param timestamp Timestamp of the trade
 * @param options Additional options (fees, id, metadata)
 * @returns Effect containing the validated buy trade
 */
export const createBuyTrade = (
  price: number,
  volume: number,
  timestamp: number,
  options?: {
    fees?: number
    id?: string
    metadata?: Record<string, unknown>
  },
): Effect.Effect<Trade, InvalidTradeError, never> =>
  createTrade({
    direction: TradeDirection.Buy,
    price,
    volume,
    timestamp,
    fees: options?.fees ?? 0,
    id: options?.id,
    metadata: options?.metadata,
  })

/**
 * Creates a sell trade
 * @param price Price at which to sell
 * @param volume Volume to sell
 * @param timestamp Timestamp of the trade
 * @param options Additional options (fees, id, metadata)
 * @returns Effect containing the validated sell trade
 */
export const createSellTrade = (
  price: number,
  volume: number,
  timestamp: number,
  options?: {
    fees?: number
    id?: string
    metadata?: Record<string, unknown>
  },
): Effect.Effect<Trade, InvalidTradeError, never> =>
  createTrade({
    direction: TradeDirection.Sell,
    price,
    volume,
    timestamp,
    fees: options?.fees ?? 0,
    id: options?.id,
    metadata: options?.metadata,
  })

/**
 * Calculate the cost of a trade (price * volume)
 * @param trade The trade to calculate cost for
 * @returns The cost of the trade
 */
export const calculateTradeCost = (trade: Trade): number => {
  return trade.price * trade.volume
}

/**
 * Calculate the fee amount for a trade
 * @param trade The trade to calculate fees for
 * @returns The fee amount
 */
export const calculateTradeFees = (trade: Trade): number => {
  return calculateTradeCost(trade) * trade.fees
}

/**
 * Calculate the total cost of a trade including fees
 * @param trade The trade to calculate total cost for
 * @returns The total cost including fees
 */
export const calculateTotalTradeCost = (trade: Trade): number => {
  const cost = calculateTradeCost(trade)
  const fees = calculateTradeFees(trade)
  return trade.direction === TradeDirection.Buy ? cost + fees : cost - fees
}
