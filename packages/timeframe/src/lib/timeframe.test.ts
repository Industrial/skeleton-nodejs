import { describe, expect, it } from 'bun:test'
import { Effect as Fx } from 'effect'

import {
  add,
  between,
  fromMs,
  millisecondsUntilNextTimeframe,
  start,
  subtract,
  toMs,
} from './timeframe.ts'

describe('timeframe', () => {
  describe('toMs', () => {
    describe('When the number is a negative number', () => {
      it('should return a negative number', () => {
        const result = Fx.runSync(toMs('-1s'))
        expect(result).toStrictEqual(-1000)
      })
    })

    describe('When the timeframe is valid', () => {
      it('should return the correct value', () => {
        const result = Fx.runSync(toMs('1s'))
        expect(result).toStrictEqual(1000)
      })
    })
  })

  describe('fromMs', () => {
    describe('When the number is negative', () => {
      it('should return a negative timeframe', () => {
        const result = Fx.runSync(fromMs(-1000, 's'))
        expect(result).toStrictEqual('-1s')
      })
    })

    describe('When the timeframe is valid', () => {
      it('should return the correct value', () => {
        const result = Fx.runSync(fromMs(1000, 's'))
        expect(result).toStrictEqual('1s')
      })
    })
  })

  describe('add', () => {
    it('should add timeframes to dates correctly', () => {
      const date = new Date()
      const result = Fx.runSync(add('1s', date))
      const expected = new Date(date.valueOf() + 1000)
      expect(result).toStrictEqual(expected)
    })
  })

  describe('subtract', () => {
    it('should subtract timeframes from dates correctly', () => {
      const date = new Date()
      const result = Fx.runSync(subtract('1s', date))
      const expected = new Date(date.valueOf() - 1000)
      expect(result).toStrictEqual(expected)
    })
  })

  describe('start', () => {
    describe('When the timeframe is 1 hour', () => {
      const timeframe = '1h'

      describe('When the date is 1 second after the start of the timeframe', () => {
        it('should return the start of the timeframe', () => {
          const date = new Date('2000-01-01T00:00:01.000Z')
          const result = Fx.runSync(start(timeframe, date))
          const expected = new Date('2000-01-01T00:00:00.000Z')
          expect(result).toStrictEqual(expected)
        })
      })

      describe('When the date is 1 second before the start of the next timeframe', () => {
        it('should return the start of the timeframe', () => {
          const date = new Date('2000-01-01T00:59:59.000Z')
          const result = Fx.runSync(start(timeframe, date))
          const expected = new Date('2000-01-01T00:00:00.000Z')
          expect(result).toStrictEqual(expected)
        })
      })
    })
  })

  describe('millisecondsUntilNextTimeframe', () => {
    describe('When the timeframe is 1 hour', () => {
      const timeframe = '1h'

      describe('When the date is 1 second after the start of the timeframe', () => {
        it('should return the milliseconds until the next timeframe', () => {
          const date = new Date('2000-01-01T00:00:01.000Z')
          const result = Fx.runSync(millisecondsUntilNextTimeframe(timeframe, date))
          const expected = 3599000
          expect(result).toStrictEqual(expected)
        })
      })

      describe('When the date is 1 second before the start of the next timeframe', () => {
        it('should return the milliseconds until the next timeframe', () => {
          const date = new Date('2000-01-01T00:59:59.000Z')
          const result = Fx.runSync(millisecondsUntilNextTimeframe(timeframe, date))
          const expected = 1000
          expect(result).toStrictEqual(expected)
        })
      })
    })
  })

  describe('between', () => {
    describe('When the timeframe is 1 hour', () => {
      const timeframe = '1h'

      describe('When the end date is within 1 hour after the start of the timeframe', () => {
        it('should generate a range of dates correctly', () => {
          const startDate = new Date('2000-01-01T00:00:00.000Z')
          const endDate = new Date('2000-01-01T00:30:00.000Z')
          const result = Fx.runSync(between(timeframe, startDate, endDate))
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
          ]
          expect(result).toStrictEqual(expected)
        })
      })

      describe('When the end date is 1 hour after the start of the timeframe', () => {
        it('should generate a range of dates correctly', () => {
          const startDate = new Date('2000-01-01T00:00:00.000Z')
          const endDate = new Date('2000-01-01T01:00:00.000Z')
          const result = Fx.runSync(between(timeframe, startDate, endDate))
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
          ]
          expect(result).toStrictEqual(expected)
        })
      })

      describe('When the end date is 2 hours after the start of the timeframe', () => {
        it('should generate a range of dates correctly', () => {
          const startDate = new Date('2000-01-01T00:00:00.000Z')
          const endDate = new Date('2000-01-01T02:00:00.000Z')
          const result = Fx.runSync(between(timeframe, startDate, endDate))
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
            new Date('2000-01-01T01:00:00.000Z'),
          ]
          expect(result).toStrictEqual(expected)
        })
      })
    })
  })
})
