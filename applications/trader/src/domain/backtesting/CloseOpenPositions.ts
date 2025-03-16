import { Schema } from 'effect'
import { BacktestParametersSchema } from './BacktestParameters'
import { BacktestStateSchema } from './BacktestState'

/**
 * Schema for close open positions input
 */
export const CloseOpenPositionsInputSchema = Schema.Struct({
  /** Current backtest state */
  state: BacktestStateSchema,

  /** Last candlestick in the backtest */
  lastCandlestick: Schema.Any, // We'll validate this separately

  /** Backtest configuration parameters */
  parameters: BacktestParametersSchema,
})

/**
 * Type for close open positions input
 */
export type CloseOpenPositionsInput = Schema.Schema.Type<
  typeof CloseOpenPositionsInputSchema
>
