import { Schema } from 'effect'

/**
 * Schema for trade metadata
 */
export const TradeMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated trade metadata
 */
export type TradeMetadataType = Schema.Schema.Type<typeof TradeMetadataSchema>
