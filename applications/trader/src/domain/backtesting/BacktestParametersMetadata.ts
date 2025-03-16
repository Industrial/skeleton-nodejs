import { Schema } from 'effect'

/**
 * Schema for backtest parameters metadata
 */
export const BacktestParametersMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated backtest parameters metadata
 */
export type BacktestParametersMetadataType = Schema.Schema.Type<
  typeof BacktestParametersMetadataSchema
>
