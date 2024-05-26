import { describe, expect, it } from 'bun:test'
import { Effect as E } from 'effect'

import { getNextPosition, normalizePositions, Position } from './position.js'

describe('position', () => {
  describe('getNextPosition', () => {
    describe('When the current position is not provided', () => {
      describe('When the next position is Buy', () => {
        it('should return the initial transition for Buy', () => {
          const actual = E.runSync(getNextPosition(Position.Buy))
          const expected: [Position, Position] = [Position.Buy, Position.Buy]
          expect(actual).toStrictEqual(expected)
        })
      })

      describe('When the next position is Sell', () => {
        it('should return the initial transition for Sell', () => {
          const actual = E.runSync(getNextPosition(Position.Sell))
          const expected: [Position, Position] = [Position.Hold, Position.Hold]
          expect(actual).toStrictEqual(expected)
        })
      })

      describe('When the next position is Hold', () => {
        it('should return the initial transition for Hold', () => {
          const actual = E.runSync(getNextPosition(Position.Hold))
          const expected: [Position, Position] = [Position.Hold, Position.Hold]
          expect(actual).toStrictEqual(expected)
        })
      })
    })

    describe('When the current position is provided', () => {
      describe('When the current position is Buy', () => {
        describe('When the next position is Buy', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Buy, Position.Buy))
            const expected: [Position, Position] = [Position.Buy, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Sell', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Sell, Position.Buy))
            const expected: [Position, Position] = [Position.Sell, Position.Sell]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Hold', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Hold, Position.Buy))
            const expected: [Position, Position] = [Position.Buy, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })
      })

      describe('When the current position is Sell', () => {
        describe('When the next position is Buy', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Buy, Position.Sell))
            const expected: [Position, Position] = [Position.Buy, Position.Buy]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Sell', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Sell, Position.Sell))
            const expected: [Position, Position] = [Position.Sell, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Hold', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Hold, Position.Sell))
            const expected: [Position, Position] = [Position.Sell, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })
      })

      describe('When the current position is Hold', () => {
        describe('When the next position is Buy', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Buy, Position.Hold))
            const expected: [Position, Position] = [Position.Buy, Position.Buy]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Sell', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Sell, Position.Hold))
            const expected: [Position, Position] = [Position.Hold, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })

        describe('When the next position is Hold', () => {
          it('should return the next position based on the transition from Buy', () => {
            const actual = E.runSync(getNextPosition(Position.Hold, Position.Hold))
            const expected: [Position, Position] = [Position.Hold, Position.Hold]
            expect(actual).toStrictEqual(expected)
          })
        })
      })
    })
  })

  describe('normalizePositions', () => {
    describe('with an empty array', () => {
      it('should return an empty array', () => {
        const actual = normalizePositions([])
        const expected: Array<Position> = []
        expect(actual).toStrictEqual(expected)
      })
    })

    describe('with an array of Positions', () => {
      it('should return an array with the next Positions based on the transition rules', () => {
        expect(normalizePositions([Position.Buy, Position.Buy, Position.Buy]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Buy, Position.Buy, Position.Sell]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Sell])
        expect(normalizePositions([Position.Buy, Position.Buy, Position.Hold]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Buy, Position.Sell, Position.Buy]))
          .toStrictEqual([Position.Buy, Position.Sell, Position.Buy])
        expect(normalizePositions([Position.Buy, Position.Hold, Position.Buy]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Buy, Position.Sell, Position.Hold]))
          .toStrictEqual([Position.Buy, Position.Sell, Position.Hold])
        expect(normalizePositions([Position.Buy, Position.Sell, Position.Sell]))
          .toStrictEqual([Position.Buy, Position.Sell, Position.Hold])
        expect(normalizePositions([Position.Buy, Position.Hold, Position.Sell]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Sell])
        expect(normalizePositions([Position.Buy, Position.Hold, Position.Hold]))
          .toStrictEqual([Position.Buy, Position.Hold, Position.Hold])

        expect(normalizePositions([Position.Sell, Position.Sell, Position.Sell]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Sell, Position.Sell, Position.Buy]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Buy])
        expect(normalizePositions([Position.Sell, Position.Buy, Position.Sell]))
          .toStrictEqual([Position.Hold, Position.Buy, Position.Sell])
        expect(normalizePositions([Position.Sell, Position.Buy, Position.Buy]))
          .toStrictEqual([Position.Hold, Position.Buy, Position.Hold])
        expect(normalizePositions([Position.Sell, Position.Sell, Position.Hold]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Sell, Position.Hold, Position.Sell]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Sell, Position.Hold, Position.Buy]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Buy])
        expect(normalizePositions([Position.Sell, Position.Buy, Position.Hold]))
          .toStrictEqual([Position.Hold, Position.Buy, Position.Hold])

        expect(normalizePositions([Position.Hold, Position.Buy, Position.Sell]))
          .toStrictEqual([Position.Hold, Position.Buy, Position.Sell])
        expect(normalizePositions([Position.Hold, Position.Sell, Position.Buy]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Buy])
        expect(normalizePositions([Position.Hold, Position.Sell, Position.Sell]))
          .toStrictEqual([Position.Hold, Position.Hold, Position.Hold])
        expect(normalizePositions([Position.Hold, Position.Buy, Position.Buy]))
          .toStrictEqual([Position.Hold, Position.Buy, Position.Hold])
      })
    })
  })
})
