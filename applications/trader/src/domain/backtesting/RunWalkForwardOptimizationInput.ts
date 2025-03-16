import { Schema } from 'effect'
import { BacktestParametersSchema } from './BacktestParameters'

/**
 * Schema for run walk-forward optimization input
 */
export const RunWalkForwardOptimizationInputSchema = Schema.Struct({
  /** Trading strategy to test */
  strategy: Schema.Any, // We'll validate this separately

  /** Historical price data */
  candlesticks: Schema.Array(Schema.Any), // We'll validate this separately

  /** List of backtest configuration parameters to optimize */
  parametersList: Schema.Array(BacktestParametersSchema),

  /** Percentage of data to use for in-sample optimization */
  inSamplePercentage: Schema.Number.pipe(
    Schema.filter((value) => value > 0 && value < 100, {
      message: () => 'In-sample percentage must be between 0 and 100',
    }),
  ),

  /** Number of folds for walk-forward optimization */
  numFolds: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Number of folds must be positive',
    }),
  ),

  /** Optional prefix for backtest names */
  namePrefix: Schema.optional(Schema.String),
})

/**
 * Type for run walk-forward optimization input
 */
export type RunWalkForwardOptimizationInput = Schema.Schema.Type<
  typeof RunWalkForwardOptimizationInputSchema
>
