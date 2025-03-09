/**
 * @module BacktestParameters
 * @description Domain model for backtest parameters.
 * Provides configuration options for backtesting trading strategies.
 */

import { Schema } from 'effect'

/**
 * Enum for position sizing methods
 */
export enum PositionSizingMethod {
  /** Fixed position size (e.g., always 1 BTC) */
  Fixed = 'fixed',

  /** Percentage of capital (e.g., 10% of available capital) */
  PercentageOfCapital = 'percentageOfCapital',

  /** Risk-based sizing (e.g., risk 1% of capital per trade) */
  RiskBased = 'riskBased',

  /** Kelly criterion sizing */
  Kelly = 'kelly',
}

/**
 * Schema for position sizing method
 */
export const PositionSizingMethodSchema = Schema.Enums(PositionSizingMethod)

/**
 * Type for validated position sizing method
 */
export type PositionSizingMethodType = Schema.Schema.Type<
  typeof PositionSizingMethodSchema
>

/**
 * Schema for backtest parameters metadata
 */
export const BacktestParametersMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated backtest parameters metadata
 */
export type BacktestParametersMetadataType = Schema.Schema.Type<
  typeof BacktestParametersMetadataSchema
>

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

/**
 * Schema for backtest parameters input
 */
export const BacktestParametersInputSchema = Schema.Struct({
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
 * Type for backtest parameters input
 */
export type BacktestParametersInput = Schema.Schema.Type<
  typeof BacktestParametersInputSchema
>

/**
 * Schema for default backtest parameters
 */
export const DefaultBacktestParametersSchema = Schema.Struct({
  initialCapital: Schema.Literal(10000),
  feeRate: Schema.Literal(0.001), // 0.1%
  slippageRate: Schema.Literal(0.001), // 0.1%
  positionSizingMethod: Schema.Literal(
    PositionSizingMethod.PercentageOfCapital,
  ),
  positionSizeValue: Schema.Literal(10), // 10% of capital
  reinvestProfits: Schema.Literal(true),
  maxConcurrentPositions: Schema.Literal(0), // unlimited
})

/**
 * Type for default backtest parameters
 */
export type DefaultBacktestParameters = Schema.Schema.Type<
  typeof DefaultBacktestParametersSchema
>
