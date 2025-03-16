import { Schema } from 'effect'
import { TradeDirectionSchema } from './TradeDirection'
import { TradeMetadataSchema } from './TradeMetadata'

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
