import { Schema } from 'effect'
import { PerformanceMetricsMetadataSchema } from './PerformanceMetricsMetadata'

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
