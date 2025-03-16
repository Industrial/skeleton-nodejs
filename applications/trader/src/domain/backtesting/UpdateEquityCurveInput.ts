import { Schema } from 'effect'
import { BacktestStateSchema } from './BacktestState'

/**
 * Schema for update equity curve input
 */
export const UpdateEquityCurveInputSchema = Schema.Struct({
  /** Current backtest state */
  state: BacktestStateSchema,

  /** Current timestamp */
  timestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Timestamp must be positive',
    }),
  ),
})

/**
 * Type for update equity curve input
 */
export type UpdateEquityCurveInput = Schema.Schema.Type<
  typeof UpdateEquityCurveInputSchema
>
