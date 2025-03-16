import { Schema } from 'effect'

/**
 * Schema for initial backtest state input
 */
export const InitialBacktestStateInputSchema = Schema.Struct({
  /** Initial capital for the backtest */
  initialCapital: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Initial capital must be positive',
    }),
  ),
})

/**
 * Type for initial backtest state input
 */
export type InitialBacktestStateInput = Schema.Schema.Type<
  typeof InitialBacktestStateInputSchema
>
