import { Schema } from 'effect'
import { BacktestParametersSchema } from './BacktestParameters'
import { BacktestStateSchema } from './BacktestState'

/**
 * Schema for process signal input
 */
export const ProcessSignalInputSchema = Schema.Struct({
  /** Signal to process */
  signal: Schema.Any, // We'll validate this separately

  /** Current candlestick */
  candlestick: Schema.Any, // We'll validate this separately

  /** Backtest configuration parameters */
  parameters: BacktestParametersSchema,

  /** Current backtest state */
  state: BacktestStateSchema,
})

/**
 * Type for process signal input
 */
export type ProcessSignalInput = Schema.Schema.Type<
  typeof ProcessSignalInputSchema
>
