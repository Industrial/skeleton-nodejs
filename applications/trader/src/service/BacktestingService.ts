/**
 * @module BacktestingService
 * @description Service interface for backtesting trading strategies.
 * Provides methods for running backtests and analyzing results.
 */

import { Data, Effect } from 'effect'
import type { Candlestick } from '../market-data/Candlestick'
import type { Strategy } from '../strategy/Strategy'
import type { BacktestParameters } from './BacktestParameters'
import type { BacktestResult } from './BacktestResult'
import type { Position } from './Position'
import type { Trade } from './Trade'

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
export interface BacktestingService {
  /**
   * Run a backtest with the specified strategy, candlesticks, and parameters
   * @param strategy The trading strategy to test
   * @param candlesticks Historical price data to test against
   * @param parameters Backtest configuration parameters
   * @param name Optional name for the backtest
   * @param description Optional description for the backtest
   * @returns Effect containing the backtest result
   */
  runBacktest: (
    strategy: Strategy,
    candlesticks: Candlestick[],
    parameters: BacktestParameters,
    name?: string,
    description?: string,
  ) => Effect.Effect<BacktestResult, BacktestingError, never>

  /**
   * Run multiple backtests with different parameters
   * @param strategy The trading strategy to test
   * @param candlesticks Historical price data to test against
   * @param parametersList List of backtest configuration parameters
   * @param namePrefix Optional prefix for backtest names
   * @returns Effect containing an array of backtest results
   */
  runParameterSweep: (
    strategy: Strategy,
    candlesticks: Candlestick[],
    parametersList: BacktestParameters[],
    namePrefix?: string,
  ) => Effect.Effect<BacktestResult[], BacktestingError, never>

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
  runWalkForwardOptimization: (
    strategy: Strategy,
    candlesticks: Candlestick[],
    parametersList: BacktestParameters[],
    inSamplePercentage: number,
    numFolds: number,
    namePrefix?: string,
  ) => Effect.Effect<BacktestResult, BacktestingError, never>

  /**
   * Get all trades from a backtest
   * @param result The backtest result
   * @returns Array of trades
   */
  getTrades: (result: BacktestResult) => Trade[]

  /**
   * Get all positions from a backtest
   * @param result The backtest result
   * @returns Array of positions
   */
  getPositions: (result: BacktestResult) => Position[]

  /**
   * Get the equity curve from a backtest
   * @param result The backtest result
   * @returns Array of [timestamp, equity] tuples
   */
  getEquityCurve: (result: BacktestResult) => [number, number][]

  /**
   * Get the drawdown curve from a backtest
   * @param result The backtest result
   * @returns Array of [timestamp, drawdown percentage] tuples
   */
  getDrawdownCurve: (result: BacktestResult) => [number, number][]

  /**
   * Save a backtest result to storage
   * @param result The backtest result to save
   * @returns Effect containing the saved backtest result ID
   */
  saveBacktestResult: (
    result: BacktestResult,
  ) => Effect.Effect<string, BacktestingError, never>

  /**
   * Load a backtest result from storage
   * @param id The ID of the backtest result to load
   * @returns Effect containing the loaded backtest result
   */
  loadBacktestResult: (
    id: string,
  ) => Effect.Effect<BacktestResult, BacktestingError, never>

  /**
   * List all saved backtest results
   * @returns Effect containing an array of backtest result IDs and names
   */
  listBacktestResults: () => Effect.Effect<
    Array<{ id: string; name: string }>,
    BacktestingError,
    never
  >

  /**
   * Delete a backtest result from storage
   * @param id The ID of the backtest result to delete
   * @returns Effect containing a boolean indicating success
   */
  deleteBacktestResult: (
    id: string,
  ) => Effect.Effect<boolean, BacktestingError, never>
}

/**
 * Tag for the BacktestingService
 */
export const BacktestingServiceTag = Effect.Tag<BacktestingService>()

/**
 * Make a BacktestingService available
 * @param service The BacktestingService implementation
 * @returns A Layer that provides the BacktestingService
 */
export const provideBacktestingService = (service: BacktestingService) =>
  Effect.provideService(BacktestingServiceTag, service)

/**
 * Run a backtest with the specified strategy, candlesticks, and parameters
 * @param strategy The trading strategy to test
 * @param candlesticks Historical price data to test against
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
): Effect.Effect<BacktestResult, BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.runBacktest(strategy, candlesticks, parameters, name, description),
  )

/**
 * Run multiple backtests with different parameters
 * @param strategy The trading strategy to test
 * @param candlesticks Historical price data to test against
 * @param parametersList List of backtest configuration parameters
 * @param namePrefix Optional prefix for backtest names
 * @returns Effect containing an array of backtest results
 */
export const runParameterSweep = (
  strategy: Strategy,
  candlesticks: Candlestick[],
  parametersList: BacktestParameters[],
  namePrefix?: string,
): Effect.Effect<BacktestResult[], BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.runParameterSweep(
      strategy,
      candlesticks,
      parametersList,
      namePrefix,
    ),
  )

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
export const runWalkForwardOptimization = (
  strategy: Strategy,
  candlesticks: Candlestick[],
  parametersList: BacktestParameters[],
  inSamplePercentage: number,
  numFolds: number,
  namePrefix?: string,
): Effect.Effect<BacktestResult, BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.runWalkForwardOptimization(
      strategy,
      candlesticks,
      parametersList,
      inSamplePercentage,
      numFolds,
      namePrefix,
    ),
  )

/**
 * Get all trades from a backtest
 * @param result The backtest result
 * @returns Array of trades
 */
export const getTrades = (
  result: BacktestResult,
): Effect.Effect<Trade[], never, BacktestingService> =>
  Effect.map(BacktestingServiceTag, (service: BacktestingService) =>
    service.getTrades(result),
  )

/**
 * Get all positions from a backtest
 * @param result The backtest result
 * @returns Array of positions
 */
export const getPositions = (
  result: BacktestResult,
): Effect.Effect<Position[], never, BacktestingService> =>
  Effect.map(BacktestingServiceTag, (service: BacktestingService) =>
    service.getPositions(result),
  )

/**
 * Get the equity curve from a backtest
 * @param result The backtest result
 * @returns Array of [timestamp, equity] tuples
 */
export const getEquityCurve = (
  result: BacktestResult,
): Effect.Effect<[number, number][], never, BacktestingService> =>
  Effect.map(BacktestingServiceTag, (service: BacktestingService) =>
    service.getEquityCurve(result),
  )

/**
 * Get the drawdown curve from a backtest
 * @param result The backtest result
 * @returns Array of [timestamp, drawdown percentage] tuples
 */
export const getDrawdownCurve = (
  result: BacktestResult,
): Effect.Effect<[number, number][], never, BacktestingService> =>
  Effect.map(BacktestingServiceTag, (service: BacktestingService) =>
    service.getDrawdownCurve(result),
  )

/**
 * Save a backtest result to storage
 * @param result The backtest result to save
 * @returns Effect containing the saved backtest result ID
 */
export const saveBacktestResult = (
  result: BacktestResult,
): Effect.Effect<string, BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.saveBacktestResult(result),
  )

/**
 * Load a backtest result from storage
 * @param id The ID of the backtest result to load
 * @returns Effect containing the loaded backtest result
 */
export const loadBacktestResult = (
  id: string,
): Effect.Effect<BacktestResult, BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.loadBacktestResult(id),
  )

/**
 * List all saved backtest results
 * @returns Effect containing an array of backtest result IDs and names
 */
export const listBacktestResults = (): Effect.Effect<
  Array<{ id: string; name: string }>,
  BacktestingError,
  BacktestingService
> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.listBacktestResults(),
  )

/**
 * Delete a backtest result from storage
 * @param id The ID of the backtest result to delete
 * @returns Effect containing a boolean indicating success
 */
export const deleteBacktestResult = (
  id: string,
): Effect.Effect<boolean, BacktestingError, BacktestingService> =>
  Effect.flatMap(BacktestingServiceTag, (service: BacktestingService) =>
    service.deleteBacktestResult(id),
  )
