/**
 * @module Position
 * @description Domain model for trading positions in backtesting.
 * Represents an open or closed trading position with entry and exit details.
 */

import { Schema } from 'effect'
import { PositionStatus, PositionStatusSchema } from './PositionStatus'
import type { Trade } from './Trade'
import { TradeDirection, TradeDirectionSchema, TradeSchema } from './Trade'

/**
 * Schema for position metadata
 */
export const PositionMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated position metadata
 */
export type PositionMetadataType = Schema.Schema.Type<
  typeof PositionMetadataSchema
>

/**
 * Schema for position data
 */
export const PositionSchema = Schema.Struct({
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
 * Type for validated position
 */
export type Position = Schema.Schema.Type<typeof PositionSchema>

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

/**
 * Schema for close position input parameters
 */
export const ClosePositionInputSchema = Schema.Struct({
  /** The open position to close */
  position: PositionSchema.pipe(
    Schema.filter((position) => position.status === PositionStatus.Open, {
      message: () => 'Position must be open',
    }),
  ),

  /** The trade that closes the position */
  exitTrade: TradeSchema,
})

/**
 * Type for close position input parameters
 */
export type ClosePositionInput = Schema.Schema.Type<
  typeof ClosePositionInputSchema
>
