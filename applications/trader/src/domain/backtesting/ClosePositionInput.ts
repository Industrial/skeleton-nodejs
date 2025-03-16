import { Schema } from 'effect'
import { PositionSchema } from './Position'
import { PositionStatus } from './PositionStatus'
import { TradeSchema } from './Trade'

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
