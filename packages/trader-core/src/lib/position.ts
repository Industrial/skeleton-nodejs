import { Maybe } from './maybe.ts'

export enum Position {
  'Buy' = 'Buy',
  'Sell' = 'Sell',
  'Hold' = 'Hold',
}

export const getNextPosition = (next: Position, current?: Position): [Position, Position] => {
  if (!current) {
    const transitions: Record<Position, [Position, Position]> = {
      [Position.Buy]: [Position.Buy, Position.Buy],
      [Position.Sell]: [Position.Hold, Position.Hold],
      [Position.Hold]: [Position.Hold, Position.Hold],
    }
    return transitions[next]
  }

  const transitions: Record<Position, Record<Position, [Position, Position]>> = {
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

  return transitions[current][next]
}

export const normalizePositions = (positions: Array<Position>): Array<Position> => {
  let current: Maybe<Position>
  return positions.reduce((accumulator, entry) => {
    const [_current, next] = getNextPosition(entry, current)
    current = _current
    return [...accumulator, next]
  }, [] as Array<Position>)
}
