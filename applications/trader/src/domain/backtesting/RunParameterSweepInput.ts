import { Schema } from 'effect'
import { BacktestParametersSchema } from './BacktestParameters'

/**
 * Schema for run parameter sweep input
 */
export const RunParameterSweepInputSchema = Schema.Struct({
  /** Trading strategy to test */
  strategy: Schema.Any, // We'll validate this separately

  /** Historical price data */
  candlesticks: Schema.Array(Schema.Any), // We'll validate this separately

  /** List of backtest configuration parameters */
  parametersList: Schema.Array(BacktestParametersSchema),

  /** Optional prefix for backtest names */
  namePrefix: Schema.optional(Schema.String),
})

/**
 * Type for run parameter sweep input
 */
export type RunParameterSweepInput = Schema.Schema.Type<
  typeof RunParameterSweepInputSchema
>
