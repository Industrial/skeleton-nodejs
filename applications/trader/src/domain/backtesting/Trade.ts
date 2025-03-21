/**
 * @module Trade
 * @description Domain model for trades in backtesting.
 * Represents a single buy or sell action with price, volume, and timestamp.
 */

import { Schema } from 'effect'
import { TradeDirectionSchema } from './TradeDirection'
import { TradeMetadataSchema } from './TradeMetadata'

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
