/**
 * @module BacktestResult
 * @description Domain model for backtest results.
 * Provides types and functions for representing and analyzing backtest results.
 */

import { Data, Effect, Schema } from 'effect'
import type { BacktestParameters } from './BacktestParameters'
import type { PerformanceMetrics } from './PerformanceMetrics'
import { calculatePerformanceMetrics } from './PerformanceMetrics'
import type { Position } from './Position'
import type { Trade } from './Trade'

/**
 * Error thrown when backtest result operations fail
 */
export class BacktestResultError extends Data.TaggedError(
  'BacktestResultError',
)<{
  readonly message: string
}> {}

/**
 * Schema for backtest result data
 */
export const BacktestResultSchema = Schema.Struct({
  /** Unique identifier for the backtest run */
  id: Schema.String,

  /** Name of the backtest run */
  name: Schema.String,

  /** Description of the backtest */
  description: Schema.optional(Schema.String),

  /** Parameters used for the backtest */
  parameters: Schema.Any, // We'll validate this separately

  /** Trades executed during the backtest */
  trades: Schema.Array(Schema.Any), // We'll validate this separately

  /** Positions opened during the backtest */
  positions: Schema.Array(Schema.Any), // We'll validate this separately

  /** Performance metrics calculated from the backtest */
  metrics: Schema.Any, // We'll validate this separately

  /** Start timestamp of the backtest period */
  startTimestamp: Schema.Number,

  /** End timestamp of the backtest period */
  endTimestamp: Schema.Number,

  /** Timestamp when the backtest was run */
  runTimestamp: Schema.Number,

  /** Duration of the backtest execution in milliseconds */
  executionTime: Schema.Number,

  /** Optional metadata for additional information */
  metadata: Schema.optional(
    Schema.Record({ key: Schema.String, value: Schema.Unknown }),
  ),
})

/**
 * Type for validated backtest result
 */
export type BacktestResult = Schema.Schema.Type<typeof BacktestResultSchema>

/**
 * Interface for backtest result parameters
 */
export interface BacktestResultParams {
  /** Unique identifier for the backtest run */
  id: string
  /** Name of the backtest run */
  name: string
  /** Description of the backtest */
  description?: string
  /** Parameters used for the backtest */
  parameters: BacktestParameters
  /** Trades executed during the backtest */
  trades: Trade[]
  /** Positions opened during the backtest */
  positions: Position[]
  /** Performance metrics calculated from the backtest */
  metrics: PerformanceMetrics
  /** Start timestamp of the backtest period */
  startTimestamp: number
  /** End timestamp of the backtest period */
  endTimestamp: number
  /** Timestamp when the backtest was run */
  runTimestamp: number
  /** Duration of the backtest execution in milliseconds */
  executionTime: number
  /** Optional metadata for additional information */
  metadata?: Record<string, unknown>
}

/**
 * Creates a validated backtest result
 * @param params Backtest result parameters
 * @returns Effect containing the validated backtest result
 * @throws {BacktestResultError} When parameters are invalid
 */
export const createBacktestResult = (
  params: BacktestResultParams,
): Effect.Effect<BacktestResult, BacktestResultError, never> =>
  Effect.gen(function* (_) {
    // Validate ID
    if (!params.id || params.id.trim() === '') {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'Backtest ID cannot be empty',
          }),
        ),
      )
    }

    // Validate name
    if (!params.name || params.name.trim() === '') {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'Backtest name cannot be empty',
          }),
        ),
      )
    }

    // Validate timestamps
    if (params.startTimestamp <= 0) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'Start timestamp must be positive',
          }),
        ),
      )
    }

    if (params.endTimestamp <= 0) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'End timestamp must be positive',
          }),
        ),
      )
    }

    if (params.endTimestamp <= params.startTimestamp) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'End timestamp must be after start timestamp',
          }),
        ),
      )
    }

    if (params.runTimestamp <= 0) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'Run timestamp must be positive',
          }),
        ),
      )
    }

    if (params.executionTime < 0) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: 'Execution time must be non-negative',
          }),
        ),
      )
    }

    // If all validations pass, create the result object
    // We're not using Schema.decodeSync here to avoid type compatibility issues
    return {
      id: params.id,
      name: params.name,
      description: params.description,
      parameters: params.parameters,
      trades: params.trades,
      positions: params.positions,
      metrics: params.metrics,
      startTimestamp: params.startTimestamp,
      endTimestamp: params.endTimestamp,
      runTimestamp: params.runTimestamp,
      executionTime: params.executionTime,
      metadata: params.metadata,
    } as BacktestResult
  })

/**
 * Creates a backtest result with calculated metrics
 * @param params Backtest result parameters without metrics
 * @returns Effect containing the backtest result with calculated metrics
 * @throws {BacktestResultError} When parameters are invalid
 */
export const createBacktestResultWithMetrics = (
  params: Omit<BacktestResultParams, 'metrics'>,
): Effect.Effect<BacktestResult, BacktestResultError, never> =>
  Effect.gen(function* (_) {
    try {
      // Calculate performance metrics
      const metrics = yield* _(
        calculatePerformanceMetrics(
          params.positions,
          params.parameters.initialCapital,
        ).pipe(
          Effect.mapError(
            (error) =>
              new BacktestResultError({
                message: `Failed to calculate performance metrics: ${error.message}`,
              }),
          ),
        ),
      )

      // Create backtest result with metrics
      return yield* _(
        createBacktestResult({
          ...params,
          metrics,
        }),
      )
    } catch (error) {
      return yield* _(
        Effect.fail(
          new BacktestResultError({
            message: `Failed to create backtest result: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Summarizes a backtest result into a human-readable string
 * @param result The backtest result to summarize
 * @returns A string summary of the backtest result
 */
export const summarizeBacktestResult = (result: BacktestResult): string => {
  const { metrics, trades, positions, parameters } = result
  const startDate = new Date(result.startTimestamp).toLocaleDateString()
  const endDate = new Date(result.endTimestamp).toLocaleDateString()

  return `
Backtest Summary: ${result.name}
${result.description ? `Description: ${result.description}\n` : ''}
Period: ${startDate} to ${endDate}
Initial Capital: $${parameters.initialCapital.toFixed(2)}

Performance:
- Total Return: $${metrics.totalReturn.toFixed(
    2,
  )} (${metrics.totalReturnPercentage.toFixed(2)}%)
- Win Rate: ${metrics.winRate.toFixed(2)}% (${metrics.winningTrades} wins, ${
    metrics.losingTrades
  } losses)
- Profit Factor: ${metrics.profitFactor.toFixed(2)}
- Max Drawdown: $${metrics.maxDrawdown.toFixed(
    2,
  )} (${metrics.maxDrawdownPercentage.toFixed(2)}%)
- Avg Profit: $${metrics.averageProfit.toFixed(2)}
- Avg Loss: $${metrics.averageLoss.toFixed(2)}
- Max Consecutive Wins: ${metrics.maxConsecutiveWins}
- Max Consecutive Losses: ${metrics.maxConsecutiveLosses}

Trading Activity:
- Total Trades: ${trades.length}
- Total Positions: ${positions.length}
- Avg Holding Period: ${(
    metrics.averageHoldingPeriod / (1000 * 60 * 60)
  ).toFixed(2)} hours

Backtest Parameters:
- Fee Rate: ${(parameters.feeRate * 100).toFixed(3)}%
- Slippage Rate: ${(parameters.slippageRate * 100).toFixed(3)}%
- Position Sizing: ${parameters.positionSizingMethod} (${
    parameters.positionSizeValue
  })
- Reinvest Profits: ${parameters.reinvestProfits ? 'Yes' : 'No'}
`.trim()
}

/**
 * Compares two backtest results and returns the difference
 * @param result1 First backtest result
 * @param result2 Second backtest result
 * @returns A string describing the differences between the results
 */
export const compareBacktestResults = (
  result1: BacktestResult,
  result2: BacktestResult,
): string => {
  const metrics1 = result1.metrics
  const metrics2 = result2.metrics

  const formatDiff = (
    value1: number,
    value2: number,
    label: string,
    formatFn: (n: number) => string = (n) => n.toFixed(2),
  ): string => {
    const diff = value2 - value1
    const diffStr = diff >= 0 ? `+${formatFn(diff)}` : formatFn(diff)
    return `${label}: ${formatFn(value1)} → ${formatFn(value2)} (${diffStr})`
  }

  return `
Comparison: ${result1.name} vs ${result2.name}

${formatDiff(metrics1.totalReturn, metrics2.totalReturn, 'Total Return ($)')}
${formatDiff(
  metrics1.totalReturnPercentage,
  metrics2.totalReturnPercentage,
  'Total Return (%)',
)}
${formatDiff(metrics1.winRate, metrics2.winRate, 'Win Rate (%)')}
${formatDiff(metrics1.profitFactor, metrics2.profitFactor, 'Profit Factor')}
${formatDiff(
  metrics1.maxDrawdownPercentage,
  metrics2.maxDrawdownPercentage,
  'Max Drawdown (%)',
)}
${formatDiff(metrics1.averageProfit, metrics2.averageProfit, 'Avg Profit ($)')}
${formatDiff(metrics1.averageLoss, metrics2.averageLoss, 'Avg Loss ($)')}
${formatDiff(
  metrics1.numberOfTrades,
  metrics2.numberOfTrades,
  'Number of Trades',
  (n) => n.toString(),
)}

Summary: ${
    metrics2.totalReturn > metrics1.totalReturn
      ? `${result2.name} outperformed ${result1.name} by $${(
          metrics2.totalReturn - metrics1.totalReturn
        ).toFixed(2)} (${(
          metrics2.totalReturnPercentage - metrics1.totalReturnPercentage
        ).toFixed(2)}%)`
      : `${result1.name} outperformed ${result2.name} by $${(
          metrics1.totalReturn - metrics2.totalReturn
        ).toFixed(2)} (${(
          metrics1.totalReturnPercentage - metrics2.totalReturnPercentage
        ).toFixed(2)}%)`
  }
`.trim()
}
