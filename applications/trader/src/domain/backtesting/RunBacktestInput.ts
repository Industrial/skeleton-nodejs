import { Schema } from 'effect'
import { StrategyBaseSchema } from '../strategy/Strategy'
import { BacktestParametersSchema } from './BacktestParameters'

/**
 * Schema for run backtest input
 */
export const RunBacktestInputSchema = Schema.Struct({
  /** Trading strategy to test */
  // strategy: Schema.Any, // We'll validate this separately
  strategy: StrategyBaseSchema,

  /** Historical price data */
  candlesticks: Schema.Array(Schema.Any), // We'll validate this separately

  /** Backtest configuration parameters */
  parameters: BacktestParametersSchema,

  /** Optional name for the backtest */
  name: Schema.optional(Schema.String),

  /** Optional description for the backtest */
  description: Schema.optional(Schema.String),
})

/**
 * Type for run backtest input
 */
export type RunBacktestInput = Schema.Schema.Type<typeof RunBacktestInputSchema>
