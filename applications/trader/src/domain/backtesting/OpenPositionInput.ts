import { Schema } from 'effect'
import { PositionMetadataSchema } from './PositionMetadata'
import { TradeSchema } from './Trade'

/**
 * Schema for open position input parameters
 */
export const OpenPositionInputSchema = Schema.Struct({
  /** The trade that opens the position */
  trade: TradeSchema,

  /** Optional position ID (generated if not provided) */
  id: Schema.optional(Schema.String),

  /** Optional metadata */
  metadata: Schema.optional(PositionMetadataSchema),
})

/**
 * Type for open position input parameters
 */
export type OpenPositionInput = Schema.Schema.Type<
  typeof OpenPositionInputSchema
>
