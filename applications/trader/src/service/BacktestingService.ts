/**
 * @module BacktestingService
 * @description Service interface for backtesting trading strategies.
 * Provides methods for running backtests and analyzing results.
 */

import { Context, Data, type Effect, Layer } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import type { BacktestParameters } from '../domain/backtesting/BacktestParameters'
import type { BacktestResult } from '../domain/backtesting/BacktestResult'
import type { Position } from '../domain/backtesting/Position'
import type { Trade } from '../domain/backtesting/Trade'
import type { Candlestick } from '../domain/market-data/Candlestick'
import type { Strategy } from '../domain/strategy/Strategy'
import { BacktestingServiceLive } from './BacktestingServiceLive'

/**
 * Error thrown when backtesting operations fail
 */
export class BacktestingError extends Data.TaggedError('BacktestingError')<{
  readonly message: string
  readonly cause?: unknown
}> {}

/**
 * Interface for the backtesting service
 */
export interface BacktestingServiceType {
  /**
   * Run a backtest with the specified strategy, candlesticks, and parameters
   * @param strategy The trading strategy to test
   * @param candlesticks Historical price data to test against
   * @param parameters Backtest configuration parameters
   * @param name Optional name for the backtest
   * @param description Optional description for the backtest
   * @returns Effect containing the backtest result
   */
  runBacktest(
    strategy: Strategy,
    candlesticks: Candlestick[],
    parameters: BacktestParameters,
    name?: string,
    description?: string,
  ): Effect.Effect<BacktestResult, BacktestingError | ParseError, never>

  /**
   * Run multiple backtests with different parameters
   * @param strategy The trading strategy to test
   * @param candlesticks Historical price data to test against
   * @param parametersList List of backtest configuration parameters
   * @param namePrefix Optional prefix for backtest names
   * @returns Effect containing an array of backtest results
   */
  runParameterSweep(
    strategy: Strategy,
    candlesticks: Candlestick[],
    parametersList: BacktestParameters[],
    namePrefix?: string,
  ): Effect.Effect<BacktestResult[], BacktestingError | ParseError, never>

  /**
   * Run a walk-forward optimization
   * @param strategy The trading strategy to test
   * @param candlesticks Historical price data to test against
   * @param parametersList List of backtest configuration parameters to optimize
   * @param inSamplePercentage Percentage of data to use for in-sample optimization
   * @param numFolds Number of folds for walk-forward optimization
   * @param namePrefix Optional prefix for backtest names
   * @returns Effect containing the optimized backtest result
   */
  runWalkForwardOptimization(
    strategy: Strategy,
    candlesticks: Candlestick[],
    parametersList: BacktestParameters[],
    inSamplePercentage: number,
    numFolds: number,
    namePrefix?: string,
  ): Effect.Effect<BacktestResult, BacktestingError | ParseError, never>

  /**
   * Get all trades from a backtest
   * @param result The backtest result
   * @returns Array of trades
   */
  getTrades(result: BacktestResult): Trade[]

  /**
   * Get all positions from a backtest
   * @param result The backtest result
   * @returns Array of positions
   */
  getPositions(result: BacktestResult): Position[]

  /**
   * Get the equity curve from a backtest
   * @param result The backtest result
   * @returns Array of [timestamp, equity] tuples
   */
  getEquityCurve(result: BacktestResult): [number, number][]

  /**
   * Get the drawdown curve from a backtest
   * @param result The backtest result
   * @returns Array of [timestamp, drawdown percentage] tuples
   */
  getDrawdownCurve(result: BacktestResult): [number, number][]

  /**
   * Save a backtest result to storage
   * @param result The backtest result to save
   * @returns Effect containing the saved backtest result ID
   */
  saveBacktestResult(
    result: BacktestResult,
  ): Effect.Effect<string, BacktestingError, never>

  /**
   * Load a backtest result from storage
   * @param id The ID of the backtest result to load
   * @returns Effect containing the loaded backtest result
   */
  loadBacktestResult(
    id: string,
  ): Effect.Effect<BacktestResult, BacktestingError, never>

  /**
   * List all saved backtest results
   * @returns Effect containing an array of backtest result IDs and names
   */
  listBacktestResults(): Effect.Effect<
    Array<{ id: string; name: string }>,
    BacktestingError,
    never
  >

  /**
   * Delete a backtest result from storage
   * @param id The ID of the backtest result to delete
   * @returns Effect containing a boolean indicating success
   */
  deleteBacktestResult(
    id: string,
  ): Effect.Effect<boolean, BacktestingError, never>
}

export class BacktestingService extends Context.Tag('BacktestingService')<
  BacktestingServiceType,
  BacktestingServiceType
>() {
  static Live = Layer.succeed(this, BacktestingServiceLive)
}
