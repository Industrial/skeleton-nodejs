import { Effect as E } from 'effect'

export enum Position {
  Buy = 'Buy',
  Sell = 'Sell',
  Hold = 'Hold',
}

export const initialTransition: Record<Position, [Position, Position]> = {
  [Position.Buy]: [Position.Buy, Position.Buy],
  [Position.Sell]: [Position.Hold, Position.Hold],
  [Position.Hold]: [Position.Hold, Position.Hold],
}

export const transitions: Record<Position, Record<Position, [Position, Position]>> = {
  [Position.Buy]: {
    [Position.Buy]: [Position.Buy, Position.Hold],
    [Position.Sell]: [Position.Sell, Position.Sell],
    [Position.Hold]: [Position.Buy, Position.Hold],
  },
  [Position.Sell]: {
    [Position.Buy]: [Position.Buy, Position.Buy],
    [Position.Sell]: [Position.Sell, Position.Hold],
    [Position.Hold]: [Position.Sell, Position.Hold],
  },
  [Position.Hold]: {
    [Position.Buy]: [Position.Buy, Position.Buy],
    [Position.Sell]: [Position.Hold, Position.Hold],
    [Position.Hold]: [Position.Hold, Position.Hold],
  },
}

export const getNextPosition = (next: Position, current?: Position) =>
  E.succeed(current
    ? transitions[current][next]
    : initialTransition[next])

export const normalizePositions = (positions: Array<Position>) => {
  const output: Array<Position> = []
  let currentMove: Position = Position.Sell
  for (const next of positions) {
    if (currentMove === Position.Buy) {
      if (next === Position.Buy) {
        output.push(Position.Hold)
      } else if (next === Position.Sell) {
        output.push(Position.Sell)
        currentMove = Position.Sell
      } else {
        output.push(Position.Hold)
      }
    // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
    } else if (currentMove === Position.Sell) {
      if (next === Position.Buy) {
        output.push(Position.Buy)
        currentMove = Position.Buy
      } else if (next === Position.Sell) {
        output.push(Position.Hold)
      } else {
        output.push(Position.Hold)
      }
    } else {
      output.push(Position.Hold)
    }
  }
  return output
}
