/**
 * @module BacktestParameters
 * @description Domain model for backtest parameters.
 * Provides configuration options for backtesting trading strategies.
 */

import { Data, Effect, Schema } from 'effect'

/**
 * Error thrown when backtest parameters are invalid
 */
export class InvalidBacktestParametersError extends Data.TaggedError(
  'InvalidBacktestParametersError',
)<{
  readonly message: string
}> {}

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
  positionSizingMethod: Schema.Enums(PositionSizingMethod),

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
  metadata: Schema.optional(
    Schema.Record({ key: Schema.String, value: Schema.Unknown }),
  ),
})

/**
 * Type for validated backtest parameters
 */
export type BacktestParameters = Schema.Schema.Type<
  typeof BacktestParametersSchema
>

/**
 * Interface for backtest parameters
 */
export interface BacktestParametersInput {
  /** Initial capital for the backtest */
  initialCapital: number
  /** Trading fee rate (as a decimal, e.g., 0.001 for 0.1%) */
  feeRate: number
  /** Slippage rate (as a decimal, e.g., 0.001 for 0.1%) */
  slippageRate: number
  /** Position sizing method */
  positionSizingMethod: PositionSizingMethod
  /** Position size value (interpretation depends on positionSizingMethod) */
  positionSizeValue: number
  /** Whether to reinvest profits */
  reinvestProfits: boolean
  /** Maximum number of concurrent positions (0 for unlimited) */
  maxConcurrentPositions: number
  /** Optional metadata for additional parameters */
  metadata?: Record<string, unknown>
}

/**
 * Creates validated backtest parameters
 * @param params Backtest parameters input
 * @returns Effect containing the validated backtest parameters
 * @throws {InvalidBacktestParametersError} When parameters are invalid
 */
export const createBacktestParameters = (
  params: BacktestParametersInput,
): Effect.Effect<BacktestParameters, InvalidBacktestParametersError, never> =>
  Effect.gen(function* (_) {
    // Validate initial capital
    if (params.initialCapital <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Initial capital must be positive',
          }),
        ),
      )
    }

    // Validate fee rate
    if (params.feeRate < 0) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Fee rate must be non-negative',
          }),
        ),
      )
    }

    // Validate slippage rate
    if (params.slippageRate < 0) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Slippage rate must be non-negative',
          }),
        ),
      )
    }

    // Validate position size value
    if (params.positionSizeValue <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Position size value must be positive',
          }),
        ),
      )
    }

    // Validate position sizing method specific constraints
    if (
      params.positionSizingMethod ===
        PositionSizingMethod.PercentageOfCapital &&
      params.positionSizeValue > 100
    ) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Percentage of capital cannot exceed 100%',
          }),
        ),
      )
    }

    // Validate max concurrent positions
    if (params.maxConcurrentPositions < 0) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: 'Maximum concurrent positions must be non-negative',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(BacktestParametersSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidBacktestParametersError({
            message: `Invalid backtest parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates default backtest parameters
 * @returns Effect containing the default backtest parameters
 */
export const createDefaultBacktestParameters = (): Effect.Effect<
  BacktestParameters,
  InvalidBacktestParametersError,
  never
> =>
  createBacktestParameters({
    initialCapital: 10000,
    feeRate: 0.001, // 0.1%
    slippageRate: 0.001, // 0.1%
    positionSizingMethod: PositionSizingMethod.PercentageOfCapital,
    positionSizeValue: 10, // 10% of capital
    reinvestProfits: true,
    maxConcurrentPositions: 0, // unlimited
  })

/**
 * Calculate the position size based on the backtest parameters
 * @param parameters Backtest parameters
 * @param availableCapital Available capital
 * @param price Current price
 * @param stopLoss Optional stop loss price (for risk-based sizing)
 * @returns The calculated position size
 */
export const calculatePositionSize = (
  parameters: BacktestParameters,
  availableCapital: number,
  price: number,
  stopLoss?: number,
): number => {
  switch (parameters.positionSizingMethod) {
    case PositionSizingMethod.Fixed: {
      return parameters.positionSizeValue
    }

    case PositionSizingMethod.PercentageOfCapital: {
      const capitalToRisk =
        (availableCapital * parameters.positionSizeValue) / 100
      return capitalToRisk / price
    }

    case PositionSizingMethod.RiskBased: {
      if (!stopLoss || stopLoss <= 0) {
        // Fall back to percentage of capital if no stop loss
        const fallbackCapital =
          (availableCapital * parameters.positionSizeValue) / 100
        return fallbackCapital / price
      }

      const riskPercentage = parameters.positionSizeValue
      const capitalRisked = (availableCapital * riskPercentage) / 100
      const priceDifference = Math.abs(price - stopLoss)
      const riskPerUnit = priceDifference / price

      return capitalRisked / (price * riskPerUnit)
    }

    case PositionSizingMethod.Kelly: {
      // Simple Kelly implementation (assumes 50% win rate if not provided)
      const winRate = (parameters.metadata?.winRate as number) || 0.5
      const winLossRatio = (parameters.metadata?.winLossRatio as number) || 1

      // Kelly formula: f* = (p * b - q) / b
      // where f* is the fraction of capital to bet
      // p is the probability of winning
      // q is the probability of losing (1 - p)
      // b is the win/loss ratio

      const kellyFraction =
        (winRate * winLossRatio - (1 - winRate)) / winLossRatio
      const adjustedKelly =
        Math.max(0, Math.min(kellyFraction, 1)) * parameters.positionSizeValue
      const kellyCapital = (availableCapital * adjustedKelly) / 100

      return kellyCapital / price
    }

    default: {
      return 0
    }
  }
}
