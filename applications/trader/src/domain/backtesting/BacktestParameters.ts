/**
 * @module BacktestParameters
 * @description Domain model for backtest parameters.
 * Provides configuration options for backtesting trading strategies.
 */

import { Schema } from 'effect'
import { BacktestParametersMetadataSchema } from './BacktestParametersMetadata'
import { PositionSizingMethodSchema } from './PositionSizingMethod'

/**
 * Schema for backtest parameters
 */
export const BacktestParametersSchema = Schema.Struct({
  /** Initial capital for the backtest */
  initialCapital: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Initial capital must be positive',
    }),
  ),

  /** Trading fee rate (as a decimal, e.g., 0.001 for 0.1%) */
  feeRate: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Fee rate must be non-negative',
    }),
  ),

  /** Slippage rate (as a decimal, e.g., 0.001 for 0.1%) */
  slippageRate: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Slippage rate must be non-negative',
    }),
  ),

  /** Position sizing method */
  positionSizingMethod: PositionSizingMethodSchema,

  /** Position size value (interpretation depends on positionSizingMethod) */
  positionSizeValue: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Position size value must be positive',
    }),
  ),

  /** Whether to reinvest profits */
  reinvestProfits: Schema.Boolean,

  /** Maximum number of concurrent positions (0 for unlimited) */
  maxConcurrentPositions: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Maximum concurrent positions must be non-negative',
    }),
  ),

  /** Optional metadata for additional parameters */
  metadata: Schema.optional(BacktestParametersMetadataSchema),
})

/**
 * Type for validated backtest parameters
 */
export type BacktestParameters = Schema.Schema.Type<
  typeof BacktestParametersSchema
>
