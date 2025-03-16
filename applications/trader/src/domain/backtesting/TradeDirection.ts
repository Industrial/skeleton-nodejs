import { Schema } from 'effect'

/**
 * Enum for trade directions
 */
export enum TradeDirection {
  /** Buy trade (long) */
  Buy = 'buy',

  /** Sell trade (short or exit) */
  Sell = 'sell',
}

/**
 * Schema for trade direction
 */
export const TradeDirectionSchema = Schema.Enums(TradeDirection)

/**
 * Type for validated trade direction
 */
export type TradeDirectionType = Schema.Schema.Type<typeof TradeDirectionSchema>
