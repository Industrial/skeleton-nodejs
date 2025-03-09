/**
 * @module PositionStatus
 * @description Enum for position statuses in backtesting.
 */

/**
 * Enum representing the status of a trading position
 */
export enum PositionStatus {
  /** Position is currently open */
  Open = 'open',

  /** Position has been closed */
  Closed = 'closed',
}
