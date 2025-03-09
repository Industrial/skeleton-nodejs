/**
 * @module PerformanceMetrics
 * @description Domain model for performance metrics in backtesting.
 * Provides calculations and types for evaluating trading strategy performance.
 */

import { Data, Effect, Schema } from 'effect'
import type { Position } from './Position'
import { calculatePositionPnL } from './Position'
import { PositionStatus } from './PositionStatus'
import type { Trade } from './Trade'
import { TradeDirection } from './Trade'

/**
 * Error thrown when performance metrics calculation fails
 */
export class PerformanceMetricsError extends Data.TaggedError(
  'PerformanceMetricsError',
)<{
  readonly message: string
}> {}

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
  winRate: Schema.Number,

  /** Average profit of winning trades */
  averageProfit: Schema.Number,

  /** Average loss of losing trades */
  averageLoss: Schema.Number,

  /** Profit factor (gross profit / gross loss) */
  profitFactor: Schema.Number,

  /** Maximum drawdown (largest peak-to-trough decline) */
  maxDrawdown: Schema.Number,

  /** Maximum drawdown percentage */
  maxDrawdownPercentage: Schema.Number,

  /** Sharpe ratio (risk-adjusted return) */
  sharpeRatio: Schema.Number.pipe(Schema.optional),

  /** Sortino ratio (downside risk-adjusted return) */
  sortinoRatio: Schema.Number.pipe(Schema.optional),

  /** Maximum consecutive winning trades */
  maxConsecutiveWins: Schema.Number,

  /** Maximum consecutive losing trades */
  maxConsecutiveLosses: Schema.Number,

  /** Average holding period (in milliseconds) */
  averageHoldingPeriod: Schema.Number,

  /** Optional metadata for additional metrics */
  metadata: Schema.optional(
    Schema.Record({ key: Schema.String, value: Schema.Unknown }),
  ),
})

/**
 * Type for validated performance metrics
 */
export type PerformanceMetrics = Schema.Schema.Type<
  typeof PerformanceMetricsSchema
>

/**
 * Interface for performance metrics parameters
 */
export interface PerformanceMetricsParams {
  /** Total return (profit/loss) in absolute terms */
  totalReturn: number
  /** Total return as a percentage of initial capital */
  totalReturnPercentage: number
  /** Number of trades executed */
  numberOfTrades: number
  /** Number of winning trades */
  winningTrades: number
  /** Number of losing trades */
  losingTrades: number
  /** Win rate (percentage of winning trades) */
  winRate: number
  /** Average profit of winning trades */
  averageProfit: number
  /** Average loss of losing trades */
  averageLoss: number
  /** Profit factor (gross profit / gross loss) */
  profitFactor: number
  /** Maximum drawdown (largest peak-to-trough decline) */
  maxDrawdown: number
  /** Maximum drawdown percentage */
  maxDrawdownPercentage: number
  /** Sharpe ratio (risk-adjusted return) */
  sharpeRatio?: number
  /** Sortino ratio (downside risk-adjusted return) */
  sortinoRatio?: number
  /** Maximum consecutive winning trades */
  maxConsecutiveWins: number
  /** Maximum consecutive losing trades */
  maxConsecutiveLosses: number
  /** Average holding period (in milliseconds) */
  averageHoldingPeriod: number
  /** Optional metadata for additional metrics */
  metadata?: Record<string, unknown>
}

/**
 * Creates validated performance metrics
 * @param params Performance metrics parameters
 * @returns Effect containing the validated performance metrics
 * @throws {PerformanceMetricsError} When parameters are invalid
 */
export const createPerformanceMetrics = (
  params: PerformanceMetricsParams,
): Effect.Effect<PerformanceMetrics, PerformanceMetricsError, never> =>
  Effect.gen(function* (_) {
    // Validate win rate
    if (params.winRate < 0 || params.winRate > 100) {
      return yield* _(
        Effect.fail(
          new PerformanceMetricsError({
            message: 'Win rate must be between 0 and 100',
          }),
        ),
      )
    }

    // Validate profit factor
    if (params.profitFactor < 0) {
      return yield* _(
        Effect.fail(
          new PerformanceMetricsError({
            message: 'Profit factor must be non-negative',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(PerformanceMetricsSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new PerformanceMetricsError({
            message: `Invalid performance metrics parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Calculate performance metrics from a list of closed positions
 * @param positions Array of positions (must be closed)
 * @param initialCapital Initial capital amount
 * @returns Effect containing the calculated performance metrics
 * @throws {PerformanceMetricsError} When calculation fails
 */
export const calculatePerformanceMetrics = (
  positions: Position[],
  initialCapital: number,
): Effect.Effect<PerformanceMetrics, PerformanceMetricsError, never> =>
  Effect.gen(function* (_) {
    // Validate positions
    const closedPositions = positions.filter(
      (p) => p.status === PositionStatus.Closed,
    )
    if (closedPositions.length === 0) {
      return yield* _(
        Effect.fail(
          new PerformanceMetricsError({
            message:
              'Cannot calculate performance metrics with no closed positions',
          }),
        ),
      )
    }

    // Validate initial capital
    if (initialCapital <= 0) {
      return yield* _(
        Effect.fail(
          new PerformanceMetricsError({
            message: 'Initial capital must be positive',
          }),
        ),
      )
    }

    // Calculate total return
    const totalReturn = closedPositions.reduce(
      (sum, position) => sum + calculatePositionPnL(position),
      0,
    )
    const totalReturnPercentage = (totalReturn / initialCapital) * 100

    // Calculate trade statistics
    const numberOfTrades = closedPositions.length
    const positionPnLs = closedPositions.map((p) => calculatePositionPnL(p))
    const winningPositions = closedPositions.filter(
      (p) => calculatePositionPnL(p) > 0,
    )
    const losingPositions = closedPositions.filter(
      (p) => calculatePositionPnL(p) < 0,
    )
    const winningTrades = winningPositions.length
    const losingTrades = losingPositions.length
    const winRate = (winningTrades / numberOfTrades) * 100

    // Calculate average profit and loss
    const totalProfit = winningPositions.reduce(
      (sum, p) => sum + calculatePositionPnL(p),
      0,
    )
    const totalLoss = Math.abs(
      losingPositions.reduce((sum, p) => sum + calculatePositionPnL(p), 0),
    )
    const averageProfit = winningTrades > 0 ? totalProfit / winningTrades : 0
    const averageLoss = losingTrades > 0 ? totalLoss / losingTrades : 0
    const profitFactor =
      totalLoss > 0
        ? totalProfit / totalLoss
        : totalProfit > 0
          ? Number.POSITIVE_INFINITY
          : 0

    // Calculate drawdown
    let maxDrawdown = 0
    let maxDrawdownPercentage = 0
    let peak = initialCapital
    let equity = initialCapital

    // Sort positions by open timestamp
    const sortedPositions = [...closedPositions].sort(
      (a, b) => a.openTimestamp - b.openTimestamp,
    )

    // Calculate equity curve and find maximum drawdown
    for (const position of sortedPositions) {
      const pnl = calculatePositionPnL(position)
      equity += pnl
      if (equity > peak) {
        peak = equity
      } else {
        const drawdown = peak - equity
        const drawdownPercentage = (drawdown / peak) * 100
        if (drawdown > maxDrawdown) {
          maxDrawdown = drawdown
          maxDrawdownPercentage = drawdownPercentage
        }
      }
    }

    // Calculate consecutive wins and losses
    let currentConsecutiveWins = 0
    let currentConsecutiveLosses = 0
    let maxConsecutiveWins = 0
    let maxConsecutiveLosses = 0

    for (const pnl of positionPnLs) {
      if (pnl > 0) {
        currentConsecutiveWins++
        currentConsecutiveLosses = 0
        maxConsecutiveWins = Math.max(
          maxConsecutiveWins,
          currentConsecutiveWins,
        )
      } else if (pnl < 0) {
        currentConsecutiveLosses++
        currentConsecutiveWins = 0
        maxConsecutiveLosses = Math.max(
          maxConsecutiveLosses,
          currentConsecutiveLosses,
        )
      }
    }

    // Calculate average holding period
    const holdingPeriods = closedPositions.map(
      (p) => (p.closeTimestamp || 0) - p.openTimestamp,
    )
    const totalHoldingPeriod = holdingPeriods.reduce(
      (sum, period) => sum + period,
      0,
    )
    const averageHoldingPeriod =
      numberOfTrades > 0 ? totalHoldingPeriod / numberOfTrades : 0

    // Create performance metrics
    return yield* _(
      createPerformanceMetrics({
        totalReturn,
        totalReturnPercentage,
        numberOfTrades,
        winningTrades,
        losingTrades,
        winRate,
        averageProfit,
        averageLoss,
        profitFactor,
        maxDrawdown,
        maxDrawdownPercentage,
        maxConsecutiveWins,
        maxConsecutiveLosses,
        averageHoldingPeriod,
      }),
    )
  })

/**
 * Calculate the Sharpe ratio
 * @param returns Array of period returns
 * @param riskFreeRate Risk-free rate (default: 0)
 * @returns The Sharpe ratio
 */
export const calculateSharpeRatio = (
  returns: number[],
  riskFreeRate = 0,
): number => {
  if (returns.length < 2) {
    return 0
  }

  const meanReturn = returns.reduce((sum, r) => sum + r, 0) / returns.length
  const excessReturn = meanReturn - riskFreeRate

  const squaredDeviations = returns.map((r) => (r - meanReturn) ** 2)
  const variance =
    squaredDeviations.reduce((sum, sd) => sum + sd, 0) / returns.length
  const stdDev = Math.sqrt(variance)

  return stdDev === 0 ? 0 : excessReturn / stdDev
}

/**
 * Calculate the Sortino ratio
 * @param returns Array of period returns
 * @param riskFreeRate Risk-free rate (default: 0)
 * @returns The Sortino ratio
 */
export const calculateSortinoRatio = (
  returns: number[],
  riskFreeRate = 0,
): number => {
  if (returns.length < 2) {
    return 0
  }

  const meanReturn = returns.reduce((sum, r) => sum + r, 0) / returns.length
  const excessReturn = meanReturn - riskFreeRate

  const negativeReturns = returns.filter((r) => r < 0)
  if (negativeReturns.length === 0) {
    return excessReturn > 0 ? Number.POSITIVE_INFINITY : 0
  }

  const squaredNegativeDeviations = negativeReturns.map((r) => r ** 2)
  const downVariance =
    squaredNegativeDeviations.reduce((sum, sd) => sum + sd, 0) /
    negativeReturns.length
  const downstdDev = Math.sqrt(downVariance)

  return downstdDev === 0 ? 0 : excessReturn / downstdDev
}
