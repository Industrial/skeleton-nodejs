import { describe, expect, it } from 'bun:test'

import { getNextPosition, normalizePositions, Position } from './position.js'

describe('getNextPosition', () => {
  it('should handle invalid current position', () => {
    expect(getNextPosition(Position.Buy)).toEqual([Position.Buy, Position.Buy])
  })

  it('should transition correctly for Buy position', () => {
    expect(getNextPosition(Position.Sell, Position.Buy)).toEqual([Position.Sell, Position.Sell])
    expect(getNextPosition(Position.Hold, Position.Buy)).toEqual([Position.Buy, Position.Hold])
  })

  it('should transition correctly for Sell position', () => {
    expect(getNextPosition(Position.Buy, Position.Sell)).toEqual([Position.Buy, Position.Buy])
    expect(getNextPosition(Position.Hold, Position.Sell)).toEqual([Position.Sell, Position.Hold])
  })

  it('should transition correctly for Hold position', () => {
    expect(getNextPosition(Position.Buy, Position.Hold)).toEqual([Position.Buy, Position.Buy])
    expect(getNextPosition(Position.Sell, Position.Hold)).toEqual([Position.Hold, Position.Hold])
  })
})

describe('normalizePositions', () => {
  it('should handle empty input', () => {
    expect(normalizePositions([])).toEqual([])
  })

  it('should normalize a single position', () => {
    expect(normalizePositions([Position.Buy])).toEqual([Position.Buy])
  })

  it('should normalize multiple positions', () => {
    expect(normalizePositions([Position.Buy, Position.Sell, Position.Hold])).toEqual([
      Position.Buy,
      Position.Sell,
      Position.Hold,
    ])
  })

  it('should normalize complex transitions', () => {
    expect(normalizePositions([Position.Buy, Position.Sell, Position.Buy])).toEqual([
      Position.Buy,
      Position.Sell,
      Position.Buy,
    ])
  })
})
