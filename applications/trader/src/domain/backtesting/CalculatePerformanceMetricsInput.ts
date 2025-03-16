import { Schema } from 'effect'

/**
 * Schema for calculating performance metrics input
 */
export const CalculatePerformanceMetricsInputSchema = Schema.Struct({
  /** Array of positions (must be closed) */
  positions: Schema.Array(Schema.Any), // We'll validate this separately

  /** Initial capital amount */
  initialCapital: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Initial capital must be positive',
    }),
  ),
})

/**
 * Type for calculating performance metrics input
 */
export type CalculatePerformanceMetricsInput = Schema.Schema.Type<
  typeof CalculatePerformanceMetricsInputSchema
>
