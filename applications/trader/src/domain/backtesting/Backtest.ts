/**
 * @module Backtest
 * @description Core implementation for running a backtest.
 * Processes candlesticks, generates signals, executes trades, and manages positions.
 */

import { Data, Effect } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'
import { SignalDirection } from '../strategy/Signal'
import type { Signal } from '../strategy/Signal'
import type { Strategy, StrategyExecutionError } from '../strategy/Strategy'
import type { BacktestParameters } from './BacktestParameters'
import { calculatePositionSize } from './BacktestParameters'
import type { BacktestResult } from './BacktestResult'
import { createBacktestResultWithMetrics } from './BacktestResult'
import type { Position } from './Position'
import {
  calculatePositionPnL,
  closePosition,
  createOpenPosition,
} from './Position'
import type { Trade } from './Trade'
import { TradeDirection, createBuyTrade, createSellTrade } from './Trade'

/**
 * Error thrown when backtest execution fails
 */
export class BacktestExecutionError extends Data.TaggedError(
  'BacktestExecutionError',
)<{
  readonly message: string
  readonly cause?: unknown
}> {}

/**
 * Interface for backtest state
 */
export interface BacktestState {
  /** Current equity (initial capital + profits/losses) */
  equity: number
  /** Available capital for new positions */
  availableCapital: number
  /** Currently open positions */
  openPositions: Position[]
  /** Closed positions */
  closedPositions: Position[]
  /** All executed trades */
  trades: Trade[]
  /** Equity curve [timestamp, equity] */
  equityCurve: [number, number][]
  /** Drawdown curve [timestamp, drawdownPercentage] */
  drawdownCurve: [number, number][]
  /** Peak equity (for drawdown calculation) */
  peakEquity: number
}

/**
 * Create initial backtest state
 * @param initialCapital Initial capital for the backtest
 * @returns Initial backtest state
 */
export const createInitialBacktestState = (
  initialCapital: number,
): BacktestState => ({
  equity: initialCapital,
  availableCapital: initialCapital,
  openPositions: [],
  closedPositions: [],
  trades: [],
  equityCurve: [],
  drawdownCurve: [],
  peakEquity: initialCapital,
})

/**
 * Run a backtest with the given strategy, candlesticks, and parameters
 * @param strategy Trading strategy to test
 * @param candlesticks Historical price data
 * @param parameters Backtest configuration parameters
 * @param name Optional name for the backtest
 * @param description Optional description for the backtest
 * @returns Effect containing the backtest result
 */
export const runBacktest = (
  strategy: Strategy,
  candlesticks: Candlestick[],
  parameters: BacktestParameters,
  name?: string,
  description?: string,
): Effect.Effect<BacktestResult, BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    // Validate inputs
    if (candlesticks.length === 0) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'Cannot run backtest with empty candlesticks array',
          }),
        ),
      )
    }

    // Sort candlesticks by timestamp (ascending)
    const sortedCandlesticks = [...candlesticks].sort(
      (a, b) => a.timestamp - b.timestamp,
    )

    // Create initial state
    const initialState = createInitialBacktestState(parameters.initialCapital)

    // Record start time for performance measurement
    const startTime = Date.now()

    try {
      // Generate signals from strategy
      const signals = yield* _(
        strategy.analyze(sortedCandlesticks).pipe(
          Effect.mapError(
            (error: StrategyExecutionError) =>
              new BacktestExecutionError({
                message: `Strategy execution failed: ${error.message}`,
                cause: error,
              }),
          ),
        ),
      )

      // Process signals and update state
      const finalState = yield* _(
        processSignals(signals, sortedCandlesticks, parameters, initialState),
      )

      // Calculate execution time
      const executionTime = Date.now() - startTime

      // Create backtest result
      return yield* _(
        createBacktestResultWithMetrics({
          id: `backtest-${Date.now()}-${Math.random()
            .toString(36)
            .substring(2, 9)}`,
          name: name || `${strategy.name} Backtest`,
          description: description,
          parameters,
          trades: finalState.trades,
          positions: [
            ...finalState.openPositions,
            ...finalState.closedPositions,
          ],
          startTimestamp: sortedCandlesticks[0].timestamp,
          endTimestamp:
            sortedCandlesticks[sortedCandlesticks.length - 1].timestamp,
          runTimestamp: Date.now(),
          executionTime,
          metadata: {
            equityCurve: finalState.equityCurve,
            drawdownCurve: finalState.drawdownCurve,
            finalEquity: finalState.equity,
          },
        }).pipe(
          Effect.mapError(
            (error) =>
              new BacktestExecutionError({
                message: `Failed to create backtest result: ${error.message}`,
                cause: error,
              }),
          ),
        ),
      )
    } catch (error) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: `Unexpected error in backtest execution: ${String(error)}`,
            cause: error,
          }),
        ),
      )
    }
  })

/**
 * Process signals and update backtest state
 * @param signals Signals generated by the strategy
 * @param candlesticks Historical price data
 * @param parameters Backtest configuration parameters
 * @param initialState Initial backtest state
 * @returns Effect containing the updated backtest state
 */
export const processSignals = (
  signals: Signal[],
  candlesticks: Candlestick[],
  parameters: BacktestParameters,
  initialState: BacktestState,
): Effect.Effect<BacktestState, BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    let state = initialState

    // Create a map of candlesticks by timestamp for quick lookup
    const candlestickMap = new Map<number, Candlestick>()
    for (const candle of candlesticks) {
      candlestickMap.set(candle.timestamp, candle)
    }

    // Process each signal
    for (const signal of signals) {
      const candlestick = candlestickMap.get(signal.timestamp)
      if (!candlestick) {
        continue // Skip signals without matching candlestick
      }

      // Update state with the current signal
      state = yield* _(processSignal(signal, candlestick, parameters, state))

      // Update equity curve and drawdown
      state = updateEquityCurve(state, signal.timestamp)
    }

    // Close any remaining open positions at the end of the backtest
    if (state.openPositions.length > 0 && candlesticks.length > 0) {
      const lastCandlestick = candlesticks[candlesticks.length - 1]
      state = yield* _(closeOpenPositions(state, lastCandlestick, parameters))
      state = updateEquityCurve(state, lastCandlestick.timestamp)
    }

    return state
  })

/**
 * Process a single signal and update backtest state
 * @param signal Signal to process
 * @param candlestick Current candlestick
 * @param parameters Backtest configuration parameters
 * @param state Current backtest state
 * @returns Effect containing the updated backtest state
 */
export const processSignal = (
  signal: Signal,
  candlestick: Candlestick,
  parameters: BacktestParameters,
  state: BacktestState,
): Effect.Effect<BacktestState, BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    const newState = { ...state }

    // Process buy signals
    if (signal.direction === SignalDirection.Buy) {
      // Check if we can open new positions (max concurrent positions)
      if (
        parameters.maxConcurrentPositions === 0 ||
        newState.openPositions.length < parameters.maxConcurrentPositions
      ) {
        // Calculate position size
        const positionSize = calculatePositionSize(
          parameters,
          newState.availableCapital,
          signal.price,
        )

        // Check if we have enough capital
        const tradeCost = positionSize * signal.price
        const tradeFees = tradeCost * parameters.feeRate
        const totalCost = tradeCost + tradeFees

        if (totalCost <= newState.availableCapital && positionSize > 0) {
          // Create buy trade
          const trade = yield* _(
            createBuyTrade(signal.price, positionSize, signal.timestamp, {
              fees: parameters.feeRate,
              metadata: {
                signalMetadata: signal.metadata,
              },
            }).pipe(
              Effect.mapError(
                (error) =>
                  new BacktestExecutionError({
                    message: `Failed to create buy trade: ${error.message}`,
                    cause: error,
                  }),
              ),
            ),
          )

          // Create open position
          const position = yield* _(
            createOpenPosition(trade).pipe(
              Effect.mapError(
                (error) =>
                  new BacktestExecutionError({
                    message: `Failed to create position: ${error.message}`,
                    cause: error,
                  }),
              ),
            ),
          )

          // Update state
          newState.trades = [...newState.trades, trade]
          newState.openPositions = [...newState.openPositions, position]
          newState.availableCapital -= totalCost
        }
      }
    }
    // Process sell signals
    else if (signal.direction === SignalDirection.Sell) {
      // Close any matching open positions
      for (let i = 0; i < newState.openPositions.length; i++) {
        const position = newState.openPositions[i]

        // Skip positions that don't match the signal direction
        // For a sell signal, we want to close long (buy) positions
        if (position.direction !== TradeDirection.Buy) {
          continue
        }

        // Create sell trade
        const trade = yield* _(
          createSellTrade(signal.price, position.size, signal.timestamp, {
            fees: parameters.feeRate,
            metadata: {
              signalMetadata: signal.metadata,
              positionId: position.id,
            },
          }).pipe(
            Effect.mapError(
              (error) =>
                new BacktestExecutionError({
                  message: `Failed to create sell trade: ${error.message}`,
                  cause: error,
                }),
            ),
          ),
        )

        // Close position
        const closedPosition = yield* _(
          closePosition(position, trade).pipe(
            Effect.mapError(
              (error) =>
                new BacktestExecutionError({
                  message: `Failed to close position: ${error.message}`,
                  cause: error,
                }),
            ),
          ),
        )

        // Calculate P&L
        const pnl = calculatePositionPnL(closedPosition)
        const tradeFees = trade.price * trade.volume * parameters.feeRate

        // Update state
        newState.trades = [...newState.trades, trade]
        newState.openPositions = newState.openPositions.filter(
          (p) => p.id !== position.id,
        )
        newState.closedPositions = [...newState.closedPositions, closedPosition]
        newState.equity += pnl - tradeFees

        // Update available capital
        if (parameters.reinvestProfits) {
          newState.availableCapital = newState.equity
        } else {
          newState.availableCapital +=
            position.entryPrice * position.size - tradeFees
        }
      }
    }

    return newState
  })

/**
 * Close all open positions at the end of the backtest
 * @param state Current backtest state
 * @param lastCandlestick Last candlestick in the backtest
 * @param parameters Backtest configuration parameters
 * @returns Effect containing the updated backtest state
 */
export const closeOpenPositions = (
  state: BacktestState,
  lastCandlestick: Candlestick,
  parameters: BacktestParameters,
): Effect.Effect<BacktestState, BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    const newState = { ...state }

    // Close each open position
    for (const position of state.openPositions) {
      // Create exit trade (opposite direction of position)
      const tradeDirection =
        position.direction === TradeDirection.Buy
          ? TradeDirection.Sell
          : TradeDirection.Buy

      const trade =
        tradeDirection === TradeDirection.Sell
          ? yield* _(
              createSellTrade(
                lastCandlestick.close,
                position.size,
                lastCandlestick.timestamp,
                {
                  fees: parameters.feeRate,
                  metadata: {
                    positionId: position.id,
                    forceClosed: true,
                  },
                },
              ).pipe(
                Effect.mapError(
                  (error) =>
                    new BacktestExecutionError({
                      message: `Failed to create exit trade: ${error.message}`,
                      cause: error,
                    }),
                ),
              ),
            )
          : yield* _(
              createBuyTrade(
                lastCandlestick.close,
                position.size,
                lastCandlestick.timestamp,
                {
                  fees: parameters.feeRate,
                  metadata: {
                    positionId: position.id,
                    forceClosed: true,
                  },
                },
              ).pipe(
                Effect.mapError(
                  (error) =>
                    new BacktestExecutionError({
                      message: `Failed to create exit trade: ${error.message}`,
                      cause: error,
                    }),
                ),
              ),
            )

      // Close position
      const closedPosition = yield* _(
        closePosition(position, trade).pipe(
          Effect.mapError(
            (error) =>
              new BacktestExecutionError({
                message: `Failed to close position: ${error.message}`,
                cause: error,
              }),
          ),
        ),
      )

      // Calculate P&L
      const pnl = calculatePositionPnL(closedPosition)
      const tradeFees = trade.price * trade.volume * parameters.feeRate

      // Update state
      newState.trades = [...newState.trades, trade]
      newState.closedPositions = [...newState.closedPositions, closedPosition]
      newState.equity += pnl - tradeFees
      newState.availableCapital +=
        position.entryPrice * position.size - tradeFees
    }

    // Clear open positions
    newState.openPositions = []

    return newState
  })

/**
 * Update equity curve and drawdown
 * @param state Current backtest state
 * @param timestamp Current timestamp
 * @returns Updated backtest state
 */
export const updateEquityCurve = (
  state: BacktestState,
  timestamp: number,
): BacktestState => {
  const newState = { ...state }

  // Update equity curve
  newState.equityCurve = [...newState.equityCurve, [timestamp, newState.equity]]

  // Update peak equity
  if (newState.equity > newState.peakEquity) {
    newState.peakEquity = newState.equity
  }

  // Calculate drawdown
  const drawdown =
    newState.peakEquity > 0
      ? ((newState.peakEquity - newState.equity) / newState.peakEquity) * 100
      : 0

  // Update drawdown curve
  newState.drawdownCurve = [...newState.drawdownCurve, [timestamp, drawdown]]

  return newState
}

/**
 * Run multiple backtests with different parameters
 * @param strategy Trading strategy to test
 * @param candlesticks Historical price data
 * @param parametersList List of backtest configuration parameters
 * @param namePrefix Optional prefix for backtest names
 * @returns Effect containing an array of backtest results
 */
export const runParameterSweep = (
  strategy: Strategy,
  candlesticks: Candlestick[],
  parametersList: BacktestParameters[],
  namePrefix?: string,
): Effect.Effect<BacktestResult[], BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    const results: BacktestResult[] = []

    for (let i = 0; i < parametersList.length; i++) {
      const parameters = parametersList[i]
      const name = `${namePrefix || 'Parameter Sweep'} #${i + 1}`
      const description = `Parameters: ${JSON.stringify(parameters)}`

      const result = yield* _(
        runBacktest(strategy, candlesticks, parameters, name, description),
      )
      results.push(result)
    }

    return results
  })

/**
 * Run a walk-forward optimization
 * @param strategy Trading strategy to test
 * @param candlesticks Historical price data
 * @param parametersList List of backtest configuration parameters to optimize
 * @param inSamplePercentage Percentage of data to use for in-sample optimization
 * @param numFolds Number of folds for walk-forward optimization
 * @param namePrefix Optional prefix for backtest names
 * @returns Effect containing the optimized backtest result
 */
export const runWalkForwardOptimization = (
  strategy: Strategy,
  candlesticks: Candlestick[],
  parametersList: BacktestParameters[],
  inSamplePercentage: number,
  numFolds: number,
  namePrefix?: string,
): Effect.Effect<BacktestResult, BacktestExecutionError, never> =>
  Effect.gen(function* (_) {
    // Validate inputs
    if (candlesticks.length === 0) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'Cannot run optimization with empty candlesticks array',
          }),
        ),
      )
    }

    if (parametersList.length === 0) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'Cannot run optimization with empty parameters list',
          }),
        ),
      )
    }

    if (inSamplePercentage <= 0 || inSamplePercentage >= 100) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'In-sample percentage must be between 0 and 100',
          }),
        ),
      )
    }

    if (numFolds <= 0) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'Number of folds must be positive',
          }),
        ),
      )
    }

    // Sort candlesticks by timestamp
    const sortedCandlesticks = [...candlesticks].sort(
      (a, b) => a.timestamp - b.timestamp,
    )

    // Calculate fold size
    const foldSize = Math.floor(sortedCandlesticks.length / numFolds)

    // Run walk-forward optimization
    const outOfSampleResults: BacktestResult[] = []

    for (let fold = 0; fold < numFolds; fold++) {
      // Calculate in-sample and out-of-sample ranges
      const foldStart = fold * foldSize
      const foldEnd = (fold + 1) * foldSize
      const inSampleSize = Math.floor(foldSize * (inSamplePercentage / 100))
      const inSampleEnd = foldStart + inSampleSize
      const outOfSampleStart = inSampleEnd
      const outOfSampleEnd = foldEnd

      // Extract in-sample and out-of-sample data
      const inSampleData = sortedCandlesticks.slice(foldStart, inSampleEnd)
      const outOfSampleData = sortedCandlesticks.slice(
        outOfSampleStart,
        outOfSampleEnd,
      )

      // Skip fold if not enough data
      if (inSampleData.length === 0 || outOfSampleData.length === 0) {
        continue
      }

      // Run parameter sweep on in-sample data
      const inSampleResults = yield* _(
        runParameterSweep(
          strategy,
          inSampleData,
          parametersList,
          `${namePrefix || 'WFO'} Fold ${fold + 1} In-Sample`,
        ),
      )

      // Find best parameters based on total return
      let bestParameters: BacktestParameters | undefined
      let bestReturn = Number.NEGATIVE_INFINITY

      for (const result of inSampleResults) {
        if (result.metrics.totalReturn > bestReturn) {
          bestReturn = result.metrics.totalReturn
          bestParameters = result.parameters
        }
      }

      // Run backtest with best parameters on out-of-sample data
      if (bestParameters) {
        const outOfSampleResult = yield* _(
          runBacktest(
            strategy,
            outOfSampleData,
            bestParameters,
            `${namePrefix || 'WFO'} Fold ${fold + 1} Out-of-Sample`,
            `Out-of-sample test for fold ${fold + 1}`,
          ),
        )
        outOfSampleResults.push(outOfSampleResult)
      }
    }

    // Combine out-of-sample results
    // For simplicity, we'll just return the best out-of-sample result
    // In a real implementation, you might want to combine them more intelligently
    let bestOutOfSampleResult: BacktestResult | undefined
    let bestOutOfSampleReturn = Number.NEGATIVE_INFINITY

    for (const result of outOfSampleResults) {
      if (result.metrics.totalReturn > bestOutOfSampleReturn) {
        bestOutOfSampleReturn = result.metrics.totalReturn
        bestOutOfSampleResult = result
      }
    }

    if (!bestOutOfSampleResult) {
      return yield* _(
        Effect.fail(
          new BacktestExecutionError({
            message: 'No valid out-of-sample results found',
          }),
        ),
      )
    }

    return bestOutOfSampleResult
  })
