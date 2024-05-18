import { describe, expect, test } from 'bun:test'

import { normalizePositions, Position } from './position.ts'

// describe('transformPosition function', () => {
//   test('should correctly transform positions', () => {
//     expect(transformPosition(Position.Buy, Position.Buy)).toEqual(Position.Hold)
//     expect(transformPosition(Position.Buy, Position.Sell)).toEqual(Position.Sell)
//     expect(transformPosition(Position.Buy, Position.Hold)).toEqual(Position.Hold)

//     expect(transformPosition(Position.Sell, Position.Buy)).toEqual(Position.Buy)
//     expect(transformPosition(Position.Sell, Position.Sell)).toEqual(Position.Hold)
//     expect(transformPosition(Position.Sell, Position.Hold)).toEqual(Position.Hold)

//     expect(transformPosition(Position.Hold, Position.Buy)).toEqual(Position.Buy)
//     expect(transformPosition(Position.Hold, Position.Sell)).toEqual(Position.Hold)
//     expect(transformPosition(Position.Hold, Position.Hold)).toEqual(Position.Hold)
//   })
// })

// describe('transformPositionToLong function', () => {
//   test('should correctly transform positions', () => {
//     expect(transformPositionToLong(Position.Buy, Position.Buy)).toEqual(Position.Buy)
//     expect(transformPositionToLong(Position.Buy, Position.Sell)).toEqual(Position.Sell)
//     expect(transformPositionToLong(Position.Buy, Position.Hold)).toEqual(Position.Buy)

//     expect(transformPositionToLong(Position.Sell, Position.Buy)).toEqual(Position.Buy)
//     expect(transformPositionToLong(Position.Sell, Position.Sell)).toEqual(Position.Hold)
//     expect(transformPositionToLong(Position.Sell, Position.Hold)).toEqual(Position.Hold)

//     expect(transformPositionToLong(Position.Hold, Position.Buy)).toEqual(Position.Buy)
//     expect(transformPositionToLong(Position.Hold, Position.Sell)).toEqual(Position.Hold)
//     expect(transformPositionToLong(Position.Hold, Position.Hold)).toEqual(Position.Hold)
//   })
// })

describe('normalizePositions function', () => {
  test('should correctly apply strategy to positions', () => {
    expect(normalizePositions([Position.Buy, Position.Buy])).toEqual([Position.Buy, Position.Hold])
    expect(normalizePositions([Position.Buy, Position.Sell])).toEqual([Position.Buy, Position.Sell])
    expect(normalizePositions([Position.Buy, Position.Hold])).toEqual([Position.Buy, Position.Hold])

    expect(normalizePositions([Position.Sell, Position.Buy])).toEqual([Position.Hold, Position.Buy])
    expect(normalizePositions([Position.Sell, Position.Sell])).toEqual([Position.Hold, Position.Hold])
    expect(normalizePositions([Position.Sell, Position.Hold])).toEqual([Position.Hold, Position.Hold])

    expect(normalizePositions([Position.Hold, Position.Buy])).toEqual([Position.Hold, Position.Buy])
    expect(normalizePositions([Position.Hold, Position.Sell])).toEqual([Position.Hold, Position.Hold])
    expect(normalizePositions([Position.Hold, Position.Hold])).toEqual([Position.Hold, Position.Hold])

    expect(normalizePositions([Position.Buy, Position.Hold, Position.Buy])).toEqual([
      Position.Buy,
      Position.Hold,
      Position.Hold,
    ])

    expect(normalizePositions([Position.Buy, Position.Hold, Position.Sell])).toEqual([
      Position.Buy,
      Position.Hold,
      Position.Sell,
    ])

    expect(normalizePositions([Position.Sell, Position.Hold, Position.Buy])).toEqual([
      Position.Hold,
      Position.Hold,
      Position.Buy,
    ])

    expect(normalizePositions([Position.Sell, Position.Hold, Position.Sell])).toEqual([
      Position.Hold,
      Position.Hold,
      Position.Hold,
    ])
  })
})
