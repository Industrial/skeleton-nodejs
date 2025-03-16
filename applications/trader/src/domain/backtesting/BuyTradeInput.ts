import { Schema } from 'effect'
import { TradeMetadataSchema } from './TradeMetadata'

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
