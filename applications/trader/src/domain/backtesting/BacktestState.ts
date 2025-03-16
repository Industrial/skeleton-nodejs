import { Schema } from 'effect'
import { BacktestStateMetadataSchema } from './BacktestStateMetadata'

/**
 * Schema for backtest state
 */
export const BacktestStateSchema = Schema.Struct({
  /** Current equity (initial capital + profits/losses) */
  equity: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Equity must be non-negative',
    }),
  ),

  /** Available capital for new positions */
  availableCapital: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Available capital must be non-negative',
    }),
  ),

  /** Currently open positions */
  openPositions: Schema.Array(Schema.Any), // We'll validate this separately

  /** Closed positions */
  closedPositions: Schema.Array(Schema.Any), // We'll validate this separately

  /** All executed trades */
  trades: Schema.Array(Schema.Any), // We'll validate this separately

  /** Equity curve [timestamp, equity] */
  equityCurve: Schema.Array(Schema.Tuple(Schema.Number, Schema.Number)),

  /** Drawdown curve [timestamp, drawdownPercentage] */
  drawdownCurve: Schema.Array(Schema.Tuple(Schema.Number, Schema.Number)),

  /** Peak equity (for drawdown calculation) */
  peakEquity: Schema.Number.pipe(
    Schema.nonNegative({
      message: () => 'Peak equity must be non-negative',
    }),
  ),

  /** Optional metadata for additional state information */
  metadata: Schema.optional(BacktestStateMetadataSchema),
})

/**
 * Type for validated backtest state
 */
export type BacktestState = Schema.Schema.Type<typeof BacktestStateSchema>
