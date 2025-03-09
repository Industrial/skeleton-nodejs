/**
 * @module Trade
 * @description Domain model for trades in backtesting.
 * Represents a single buy or sell action with price, volume, and timestamp.
 */

import { Schema } from 'effect'

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
 * Schema for trade direction
 */
export const TradeDirectionSchema = Schema.Enums(TradeDirection)

/**
 * Type for validated trade direction
 */
export type TradeDirectionType = Schema.Schema.Type<typeof TradeDirectionSchema>

/**
 * Schema for trade metadata
 */
export const TradeMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated trade metadata
 */
export type TradeMetadataType = Schema.Schema.Type<typeof TradeMetadataSchema>

/**
 * Schema for trade data
 */
export const TradeSchema = Schema.Struct({
  /** Direction of the trade (buy or sell) */
  direction: TradeDirectionSchema,

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
  metadata: Schema.optional(TradeMetadataSchema),
})

/**
 * Type for validated trade
 */
export type Trade = Schema.Schema.Type<typeof TradeSchema>

/**
 * Schema for trade input parameters
 */
export const TradeInputSchema = Schema.Struct({
  /** Direction of the trade (buy or sell) */
  direction: TradeDirectionSchema,

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

  /** Timestamp when the trade was executed */
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),

  /** Trading fees for this trade */
  fees: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Fees must be non-negative',
    }),
  ),

  /** Optional identifier for the trade */
  id: Schema.optional(Schema.String),

  /** Optional metadata for additional trade information */
  metadata: Schema.optional(TradeMetadataSchema),
})

/**
 * Type for trade input parameters
 */
export type TradeInput = Schema.Schema.Type<typeof TradeInputSchema>

/**
 * Schema for buy trade input parameters
 */
export const BuyTradeInputSchema = Schema.Struct({
  /** Price at which to buy */
  price: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Price must be positive',
    }),
  ),

  /** Volume to buy */
  volume: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Volume must be positive',
    }),
  ),

  /** Timestamp of the trade */
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),

  /** Optional fees */
  fees: Schema.optional(
    Schema.Number.pipe(
      Schema.nonNegative({
        message: () => 'Fees must be non-negative',
      }),
    ),
  ),

  /** Optional identifier */
  id: Schema.optional(Schema.String),

  /** Optional metadata */
  metadata: Schema.optional(TradeMetadataSchema),
})

/**
 * Type for buy trade input parameters
 */
export type BuyTradeInput = Schema.Schema.Type<typeof BuyTradeInputSchema>

/**
 * Schema for sell trade input parameters
 */
export const SellTradeInputSchema = Schema.Struct({
  /** Price at which to sell */
  price: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Price must be positive',
    }),
  ),

  /** Volume to sell */
  volume: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Volume must be positive',
    }),
  ),

  /** Timestamp of the trade */
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),

  /** Optional fees */
  fees: Schema.optional(
    Schema.Number.pipe(
      Schema.nonNegative({
        message: () => 'Fees must be non-negative',
      }),
    ),
  ),

  /** Optional identifier */
  id: Schema.optional(Schema.String),

  /** Optional metadata */
  metadata: Schema.optional(TradeMetadataSchema),
})

/**
 * Type for sell trade input parameters
 */
export type SellTradeInput = Schema.Schema.Type<typeof SellTradeInputSchema>
