/**
 * @module BacktestResult
 * @description Domain model for backtest results.
 * Provides types and schemas for representing backtest results.
 */

import { Schema } from 'effect'
import type { BacktestParameters } from './BacktestParameters'
import { BacktestParametersSchema } from './BacktestParameters'
import type { PerformanceMetrics } from './PerformanceMetrics'
import { PerformanceMetricsSchema } from './PerformanceMetrics'
import type { Position } from './Position'
import { PositionSchema } from './Position'
import type { Trade } from './Trade'
import { TradeSchema } from './Trade'

/**
 * Schema for backtest result metadata
 */
export const BacktestResultMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated backtest result metadata
 */
export type BacktestResultMetadataType = Schema.Schema.Type<
  typeof BacktestResultMetadataSchema
>

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
  parameters: BacktestParametersSchema,

  /** Trades executed during the backtest */
  trades: Schema.Array(TradeSchema),

  /** Positions opened during the backtest */
  positions: Schema.Array(PositionSchema),

  /** Performance metrics calculated from the backtest */
  metrics: PerformanceMetricsSchema,

  /** Start timestamp of the backtest period */
  startTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Start timestamp must be positive',
    }),
  ),

  /** End timestamp of the backtest period */
  endTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'End timestamp must be positive',
    }),
  ),

  /** Timestamp when the backtest was run */
  runTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Run timestamp must be positive',
    }),
  ),

  /** Duration of the backtest execution in milliseconds */
  executionTime: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Execution time must be non-negative',
    }),
  ),

  /** Optional metadata for additional information */
  metadata: Schema.optional(BacktestResultMetadataSchema),
})

/**
 * Type for validated backtest result
 */
export type BacktestResult = Schema.Schema.Type<typeof BacktestResultSchema>

/**
 * Schema for backtest result input parameters
 */
export const BacktestResultInputSchema = Schema.Struct({
  /** Unique identifier for the backtest run */
  id: Schema.String,

  /** Name of the backtest run */
  name: Schema.String,

  /** Description of the backtest */
  description: Schema.optional(Schema.String),

  /** Parameters used for the backtest */
  parameters: BacktestParametersSchema,

  /** Trades executed during the backtest */
  trades: Schema.Array(TradeSchema),

  /** Positions opened during the backtest */
  positions: Schema.Array(PositionSchema),

  /** Performance metrics calculated from the backtest */
  metrics: PerformanceMetricsSchema,

  /** Start timestamp of the backtest period */
  startTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Start timestamp must be positive',
    }),
  ),

  /** End timestamp of the backtest period */
  endTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'End timestamp must be positive',
    }),
  ),

  /** Timestamp when the backtest was run */
  runTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Run timestamp must be positive',
    }),
  ),

  /** Duration of the backtest execution in milliseconds */
  executionTime: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Execution time must be non-negative',
    }),
  ),

  /** Optional metadata for additional information */
  metadata: Schema.optional(BacktestResultMetadataSchema),
})

/**
 * Type for backtest result input parameters
 */
export type BacktestResultInput = Schema.Schema.Type<
  typeof BacktestResultInputSchema
>

/**
 * Schema for backtest result with metrics input parameters
 */
export const BacktestResultWithMetricsInputSchema = Schema.Struct({
  /** Unique identifier for the backtest run */
  id: Schema.String,

  /** Name of the backtest run */
  name: Schema.String,

  /** Description of the backtest */
  description: Schema.optional(Schema.String),

  /** Parameters used for the backtest */
  parameters: BacktestParametersSchema,

  /** Trades executed during the backtest */
  trades: Schema.Array(TradeSchema),

  /** Positions opened during the backtest */
  positions: Schema.Array(PositionSchema),

  /** Start timestamp of the backtest period */
  startTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Start timestamp must be positive',
    }),
  ),

  /** End timestamp of the backtest period */
  endTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'End timestamp must be positive',
    }),
  ),

  /** Timestamp when the backtest was run */
  runTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Run timestamp must be positive',
    }),
  ),

  /** Duration of the backtest execution in milliseconds */
  executionTime: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Execution time must be non-negative',
    }),
  ),

  /** Optional metadata for additional information */
  metadata: Schema.optional(BacktestResultMetadataSchema),
})

/**
 * Type for backtest result with metrics input parameters
 */
export type BacktestResultWithMetricsInput = Schema.Schema.Type<
  typeof BacktestResultWithMetricsInputSchema
>
