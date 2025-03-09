/**
 * @module Position
 * @description Domain model for trading positions in backtesting.
 * Represents an open or closed trading position with entry and exit details.
 */

import { Data, Effect, Schema } from 'effect'
import { PositionStatus } from './PositionStatus'
import { type InvalidTradeError, type Trade, TradeDirection } from './Trade'

/**
 * Error thrown when position parameters are invalid
 */
export class InvalidPositionError extends Data.TaggedError(
  'InvalidPositionError',
)<{
  readonly message: string
}> {}

/**
 * Schema for position data
 */
export const PositionSchema = Schema.Struct({
  /** Unique identifier for the position */
  id: Schema.String,

  /** Status of the position (open or closed) */
  status: Schema.Enums(PositionStatus),

  /** Direction of the position (long or short) */
  direction: Schema.Enums(TradeDirection),

  /** Entry price of the position */
  entryPrice: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Entry price must be positive',
    }),
  ),

  /** Size/volume of the position */
  size: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Size must be positive',
    }),
  ),

  /** Timestamp when the position was opened */
  openTimestamp: Schema.Number.pipe(
    Schema.positive({
      message: () => 'Open timestamp must be positive',
    }),
  ),

  /** Exit price of the position (if closed) */
  exitPrice: Schema.optional(
    Schema.Number.pipe(
      Schema.positive({
        message: () => 'Exit price must be positive',
      }),
    ),
  ),

  /** Timestamp when the position was closed (if closed) */
  closeTimestamp: Schema.optional(
    Schema.Number.pipe(
      Schema.positive({
        message: () => 'Close timestamp must be positive',
      }),
    ),
  ),

  /** Entry trade that opened the position */
  entryTrade: Schema.Any, // We'll validate this separately

  /** Exit trade that closed the position (if closed) */
  exitTrade: Schema.optional(Schema.Any), // We'll validate this separately

  /** Optional metadata for additional position information */
  metadata: Schema.optional(
    Schema.Record({ key: Schema.String, value: Schema.Unknown }),
  ),
})

/**
 * Type for validated position
 */
export type Position = Schema.Schema.Type<typeof PositionSchema>

/**
 * Interface for position parameters
 */
export interface PositionParams {
  /** Unique identifier for the position */
  id: string
  /** Status of the position (open or closed) */
  status: PositionStatus
  /** Direction of the position (long or short) */
  direction: TradeDirection
  /** Entry price of the position */
  entryPrice: number
  /** Size/volume of the position */
  size: number
  /** Timestamp when the position was opened */
  openTimestamp: number
  /** Exit price of the position (if closed) */
  exitPrice?: number
  /** Timestamp when the position was closed (if closed) */
  closeTimestamp?: number
  /** Entry trade that opened the position */
  entryTrade: Trade
  /** Exit trade that closed the position (if closed) */
  exitTrade?: Trade
  /** Optional metadata for additional position information */
  metadata?: Record<string, unknown>
}

/**
 * Creates a validated position
 * @param params Position parameters
 * @returns Effect containing the validated position
 * @throws {InvalidPositionError} When position parameters are invalid
 */
export const createPosition = (
  params: PositionParams,
): Effect.Effect<Position, InvalidPositionError, never> =>
  Effect.gen(function* (_) {
    // Validate entry price
    if (params.entryPrice <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: 'Entry price must be positive',
          }),
        ),
      )
    }

    // Validate size
    if (params.size <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: 'Size must be positive',
          }),
        ),
      )
    }

    // Validate open timestamp
    if (params.openTimestamp <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: 'Open timestamp must be positive',
          }),
        ),
      )
    }

    // Validate entry trade direction matches position direction
    if (params.entryTrade.direction !== params.direction) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: 'Entry trade direction must match position direction',
          }),
        ),
      )
    }

    // If position is closed, validate exit details
    if (params.status === PositionStatus.Closed) {
      // Validate exit price
      if (!params.exitPrice || params.exitPrice <= 0) {
        return yield* _(
          Effect.fail(
            new InvalidPositionError({
              message: 'Exit price must be positive for closed positions',
            }),
          ),
        )
      }

      // Validate close timestamp
      if (!params.closeTimestamp || params.closeTimestamp <= 0) {
        return yield* _(
          Effect.fail(
            new InvalidPositionError({
              message: 'Close timestamp must be positive for closed positions',
            }),
          ),
        )
      }

      // Validate close timestamp is after open timestamp
      if (params.closeTimestamp <= params.openTimestamp) {
        return yield* _(
          Effect.fail(
            new InvalidPositionError({
              message: 'Close timestamp must be after open timestamp',
            }),
          ),
        )
      }

      // Validate exit trade exists
      if (!params.exitTrade) {
        return yield* _(
          Effect.fail(
            new InvalidPositionError({
              message: 'Exit trade is required for closed positions',
            }),
          ),
        )
      }

      // Validate exit trade direction is opposite of position direction
      if (params.exitTrade.direction === params.direction) {
        return yield* _(
          Effect.fail(
            new InvalidPositionError({
              message:
                'Exit trade direction must be opposite of position direction',
            }),
          ),
        )
      }
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(PositionSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: `Invalid position parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Creates an open position from a trade
 * @param trade The trade that opens the position
 * @param id Optional position ID (generated if not provided)
 * @param metadata Optional metadata
 * @returns Effect containing the validated position
 */
export const createOpenPosition = (
  trade: Trade,
  id?: string,
  metadata?: Record<string, unknown>,
): Effect.Effect<Position, InvalidPositionError, never> =>
  createPosition({
    id:
      id ||
      `position-${Date.now()}-${Math.random().toString(36).substring(2, 9)}`,
    status: PositionStatus.Open,
    direction: trade.direction,
    entryPrice: trade.price,
    size: trade.volume,
    openTimestamp: trade.timestamp,
    entryTrade: trade,
    metadata,
  })

/**
 * Closes an open position with an exit trade
 * @param position The open position to close
 * @param exitTrade The trade that closes the position
 * @returns Effect containing the closed position
 * @throws {InvalidPositionError} When position or exit trade is invalid
 */
export const closePosition = (
  position: Position,
  exitTrade: Trade,
): Effect.Effect<Position, InvalidPositionError, never> =>
  Effect.gen(function* (_) {
    // Validate position is open
    if (position.status !== PositionStatus.Open) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message: 'Cannot close a position that is not open',
          }),
        ),
      )
    }

    // Validate exit trade direction is opposite of position direction
    if (exitTrade.direction === position.direction) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message:
              'Exit trade direction must be opposite of position direction',
          }),
        ),
      )
    }

    // Validate exit trade timestamp is after position open timestamp
    if (exitTrade.timestamp <= position.openTimestamp) {
      return yield* _(
        Effect.fail(
          new InvalidPositionError({
            message:
              'Exit trade timestamp must be after position open timestamp',
          }),
        ),
      )
    }

    // Create closed position
    return yield* _(
      createPosition({
        ...position,
        status: PositionStatus.Closed,
        exitPrice: exitTrade.price,
        closeTimestamp: exitTrade.timestamp,
        exitTrade,
      }),
    )
  })

/**
 * Calculate the profit/loss of a position
 * @param position The position to calculate P&L for
 * @returns The profit/loss amount (positive for profit, negative for loss)
 */
export const calculatePositionPnL = (position: Position): number => {
  if (position.status !== PositionStatus.Closed || !position.exitPrice) {
    return 0 // Cannot calculate P&L for open positions
  }

  const entryValue = position.entryPrice * position.size
  const exitValue = position.exitPrice * position.size

  // For long positions: exitValue - entryValue
  // For short positions: entryValue - exitValue
  return position.direction === TradeDirection.Buy
    ? exitValue - entryValue
    : entryValue - exitValue
}

/**
 * Calculate the percentage return of a position
 * @param position The position to calculate return for
 * @returns The percentage return (positive for profit, negative for loss)
 */
export const calculatePositionReturn = (position: Position): number => {
  if (position.status !== PositionStatus.Closed || !position.exitPrice) {
    return 0 // Cannot calculate return for open positions
  }

  const pnl = calculatePositionPnL(position)
  const entryValue = position.entryPrice * position.size

  return (pnl / entryValue) * 100
}

/**
 * Calculate the duration of a position in milliseconds
 * @param position The position to calculate duration for
 * @returns The duration in milliseconds
 */
export const calculatePositionDuration = (position: Position): number => {
  if (position.status !== PositionStatus.Closed || !position.closeTimestamp) {
    return Date.now() - position.openTimestamp // For open positions, use current time
  }

  return position.closeTimestamp - position.openTimestamp
}
