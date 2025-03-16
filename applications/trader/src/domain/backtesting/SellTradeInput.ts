import { Schema } from 'effect'
import { TradeMetadataSchema } from './TradeMetadata'

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
