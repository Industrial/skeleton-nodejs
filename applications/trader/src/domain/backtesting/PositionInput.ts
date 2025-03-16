import { Schema } from 'effect'
import { PositionMetadataSchema } from './PositionMetadata'
import { PositionStatusSchema } from './PositionStatus'
import { TradeSchema } from './Trade'
import { TradeDirectionSchema } from './TradeDirection'

/**
 * Schema for position input parameters
 */
export const PositionInputSchema = Schema.Struct({
  /** Unique identifier for the position */
  id: Schema.String,

  /** Status of the position (open or closed) */
  status: PositionStatusSchema,

  /** Direction of the position (long or short) */
  direction: TradeDirectionSchema,

  /** Entry price of the position */
  entryPrice: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Entry price must be positive',
    }),
  ),

  /** Size/volume of the position */
  size: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Size must be positive',
    }),
  ),

  /** Timestamp when the position was opened */
  openTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Open timestamp must be positive',
    }),
  ),

  /** Exit price of the position (if closed) */
  exitPrice: Schema.optional(
    Schema.Number.pipe(
      Schema.positive({
        message: () => 'Exit price must be positive',
      }),
    ),
  ),

  /** Timestamp when the position was closed (if closed) */
  closeTimestamp: Schema.optional(
    Schema.Number.pipe(
      Schema.positive({
        message: () => 'Close timestamp must be positive',
      }),
    ),
  ),

  /** Entry trade that opened the position */
  entryTrade: TradeSchema,

  /** Exit trade that closed the position (if closed) */
  exitTrade: Schema.optional(TradeSchema),

  /** Optional metadata for additional position information */
  metadata: Schema.optional(PositionMetadataSchema),
})

/**
 * Type for position input parameters
 */
export type PositionInput = Schema.Schema.Type<typeof PositionInputSchema>
