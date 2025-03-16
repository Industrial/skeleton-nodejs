import { Schema } from 'effect'

/**
 * Schema for position metadata
 */
export const PositionMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated position metadata
 */
export type PositionMetadataType = Schema.Schema.Type<
  typeof PositionMetadataSchema
>
