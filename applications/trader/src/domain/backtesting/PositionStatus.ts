/**
 * @module PositionStatus
 * @description Enum for position statuses in backtesting.
 */

import { Schema } from 'effect'

/**
 * Enum representing the status of a trading position
 */
export enum PositionStatus {
  /** Position is currently open */
  Open = 'open',

  /** Position has been closed */
  Closed = 'closed',
}

/**
 * Schema for position status
 */
export const PositionStatusSchema = Schema.Enums(PositionStatus)

/**
 * Type for validated position status
 */
export type PositionStatusType = Schema.Schema.Type<typeof PositionStatusSchema>
