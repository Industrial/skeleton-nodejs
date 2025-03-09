/**
 * @module PerformanceMetrics
 * @description Domain model for performance metrics in backtesting.
 * Provides calculations and types for evaluating trading strategy performance.
 */

import { Schema } from 'effect'

/**
 * Schema for performance metrics metadata
 */
export const PerformanceMetricsMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated performance metrics metadata
 */
export type PerformanceMetricsMetadataType = Schema.Schema.Type<
  typeof PerformanceMetricsMetadataSchema
>

/**
 * Schema for performance metrics data
 */
export const PerformanceMetricsSchema = Schema.Struct({
  /** Total return (profit/loss) in absolute terms */
  totalReturn: Schema.Number,

  /** Total return as a percentage of initial capital */
  totalReturnPercentage: Schema.Number,

  /** Number of trades executed */
  numberOfTrades: Schema.Number,

  /** Number of winning trades */
  winningTrades: Schema.Number,

  /** Number of losing trades */
  losingTrades: Schema.Number,

  /** Win rate (percentage of winning trades) */
  winRate: Schema.Number.pipe(
    Schema.filter((value) => value >= 0 && value <= 100, {
      message: () => 'Win rate must be between 0 and 100',
    }),
  ),

  /** Average profit of winning trades */
  averageProfit: Schema.Number,

  /** Average loss of losing trades */
  averageLoss: Schema.Number,

  /** Profit factor (gross profit / gross loss) */
  profitFactor: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Profit factor must be non-negative',
    }),
  ),

  /** Maximum drawdown (largest peak-to-trough decline) */
  maxDrawdown: Schema.Number,

  /** Maximum drawdown percentage */
  maxDrawdownPercentage: Schema.Number,

  /** Sharpe ratio (risk-adjusted return) */
  sharpeRatio: Schema.optional(Schema.Number),

  /** Sortino ratio (downside risk-adjusted return) */
  sortinoRatio: Schema.optional(Schema.Number),

  /** Maximum consecutive winning trades */
  maxConsecutiveWins: Schema.Number,

  /** Maximum consecutive losing trades */
  maxConsecutiveLosses: Schema.Number,

  /** Average holding period (in milliseconds) */
  averageHoldingPeriod: Schema.Number,

  /** Optional metadata for additional metrics */
  metadata: Schema.optional(PerformanceMetricsMetadataSchema),
})

/**
 * Type for validated performance metrics
 */
export type PerformanceMetrics = Schema.Schema.Type<
  typeof PerformanceMetricsSchema
>

/**
 * Schema for performance metrics input parameters
 */
export const PerformanceMetricsInputSchema = Schema.Struct({
  /** Total return (profit/loss) in absolute terms */
  totalReturn: Schema.Number,

  /** Total return as a percentage of initial capital */
  totalReturnPercentage: Schema.Number,

  /** Number of trades executed */
  numberOfTrades: Schema.Number,

  /** Number of winning trades */
  winningTrades: Schema.Number,

  /** Number of losing trades */
  losingTrades: Schema.Number,

  /** Win rate (percentage of winning trades) */
  winRate: Schema.Number.pipe(
    Schema.filter((value) => value >= 0 && value <= 100, {
      message: () => 'Win rate must be between 0 and 100',
    }),
  ),

  /** Average profit of winning trades */
  averageProfit: Schema.Number,

  /** Average loss of losing trades */
  averageLoss: Schema.Number,

  /** Profit factor (gross profit / gross loss) */
  profitFactor: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Profit factor must be non-negative',
    }),
  ),

  /** Maximum drawdown (largest peak-to-trough decline) */
  maxDrawdown: Schema.Number,

  /** Maximum drawdown percentage */
  maxDrawdownPercentage: Schema.Number,

  /** Sharpe ratio (risk-adjusted return) */
  sharpeRatio: Schema.optional(Schema.Number),

  /** Sortino ratio (downside risk-adjusted return) */
  sortinoRatio: Schema.optional(Schema.Number),

  /** Maximum consecutive winning trades */
  maxConsecutiveWins: Schema.Number,

  /** Maximum consecutive losing trades */
  maxConsecutiveLosses: Schema.Number,

  /** Average holding period (in milliseconds) */
  averageHoldingPeriod: Schema.Number,

  /** Optional metadata for additional metrics */
  metadata: Schema.optional(PerformanceMetricsMetadataSchema),
})

/**
 * Type for performance metrics input parameters
 */
export type PerformanceMetricsInput = Schema.Schema.Type<
  typeof PerformanceMetricsInputSchema
>

/**
 * Schema for calculating performance metrics input
 */
export const CalculatePerformanceMetricsInputSchema = Schema.Struct({
  /** Array of positions (must be closed) */
  positions: Schema.Array(Schema.Any), // We'll validate this separately

  /** Initial capital amount */
  initialCapital: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Initial capital must be positive',
    }),
  ),
})

/**
 * Type for calculating performance metrics input
 */
export type CalculatePerformanceMetricsInput = Schema.Schema.Type<
  typeof CalculatePerformanceMetricsInputSchema
>
