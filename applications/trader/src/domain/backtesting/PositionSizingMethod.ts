import { Schema } from 'effect'

/**
 * Enum for position sizing methods
 */
export enum PositionSizingMethod {
  /** Fixed position size (e.g., always 1 BTC) */
  Fixed = 'fixed',

  /** Percentage of capital (e.g., 10% of available capital) */
  PercentageOfCapital = 'percentageOfCapital',

  /** Risk-based sizing (e.g., risk 1% of capital per trade) */
  RiskBased = 'riskBased',

  /** Kelly criterion sizing */
  Kelly = 'kelly',
}

/**
 * Schema for position sizing method
 */
export const PositionSizingMethodSchema = Schema.Enums(PositionSizingMethod)

/**
 * Type for validated position sizing method
 */
export type PositionSizingMethodType = Schema.Schema.Type<
  typeof PositionSizingMethodSchema
>
