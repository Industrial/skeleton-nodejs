import { Schema } from 'effect'

/**
 * Schema for performance metrics metadata
 */
export const PerformanceMetricsMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated performance metrics metadata
 */
export type PerformanceMetricsMetadataType = Schema.Schema.Type<
  typeof PerformanceMetricsMetadataSchema
>
