import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'

/**
 * Schema for default backtest parameters
 */
export const DefaultBacktestParametersSchema = Schema.Struct({
  initialCapital: Schema.Literal(10000),
  feeRate: Schema.Literal(0.001), // 0.1%
  slippageRate: Schema.Literal(0.001), // 0.1%
  positionSizingMethod: Schema.Literal(
    PositionSizingMethod.PercentageOfCapital,
  ),
  positionSizeValue: Schema.Literal(10), // 10% of capital
  reinvestProfits: Schema.Literal(true),
  maxConcurrentPositions: Schema.Literal(0), // unlimited
})

/**
 * Type for default backtest parameters
 */
export type DefaultBacktestParameters = Schema.Schema.Type<
  typeof DefaultBacktestParametersSchema
>
