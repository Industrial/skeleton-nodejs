import { Schema } from 'effect'

/**
 * Schema for backtest state metadata
 */
export const BacktestStateMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated backtest state metadata
 */
export type BacktestStateMetadataType = Schema.Schema.Type<
  typeof BacktestStateMetadataSchema
>
