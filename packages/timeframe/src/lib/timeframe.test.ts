import * as O from 'fp-ts/Option'
import { describe, expect, it } from 'vitest'

import {
  add,
  between,
  fromMs,
  millisecondsUntilNextTimeframe,
  start,
  subtract,
  subtractSeconds,
  toMs,
} from './timeframe.ts'

describe('timeframe', () => {
  describe('toMs', () => {
    describe('When the number is a negative number', () => {
      it('should return a negative number', () => {
        const expected = O.some(-1000)
        const result = toMs('-1s')
        expect(result).toStrictEqual(expected)
      })
    })

    describe('When the timeframe is valid', () => {
      it('should return the correct value', () => {
        const expected = O.some(1000)
        const result = toMs('1s')
        expect(result).toStrictEqual(expected)
      })
    })
  })

  describe('fromMs', () => {
    describe('When the number is negative', () => {
      it('should return a negative timeframe', () => {
        const expected = O.some('-1s')
        const result = fromMs(-1000, 's')
        expect(result).toStrictEqual(expected)
      })
    })

    describe('When the timeframe is valid', () => {
      it('should return the correct value', () => {
        const expected = O.some('1s')
        const result = fromMs(1000, 's')
        expect(result).toStrictEqual(expected)
      })
    })
  })

  describe('add', () => {
    it('should add timeframes to dates correctly', () => {
      const date = new Date()
      const expected = new Date(date.valueOf() + 1000)
      const result = add('1s', date)
      expect(result).toStrictEqual(O.some(expected))
    })
  })

  describe('subtract', () => {
    it('should add timeframes to dates correctly', () => {
      const date = new Date()
      const expected = new Date(date.valueOf() - 1000)
      const result = subtract('1s', date)
      expect(result).toStrictEqual(O.some(expected))
    })
  })

  describe('subtractSeconds', () => {
    describe('When passed no amount', () => {
      it('should subtract 1 second', () => {
        const date = new Date()
        const expected = new Date(date.valueOf() - 1000)
        const result = subtractSeconds(date)
        expect(result).toStrictEqual(O.some(expected))
      })
    })
    describe('When passed an amount', () => {
      it('should subtract seconds correctly', () => {
        const date = new Date()
        const expected = new Date(date.valueOf() - 5000)
        const result = subtractSeconds(date, 5)
        expect(result).toStrictEqual(O.some(expected))
      })
    })
  })

  describe('start', () => {
    describe('When the timeframe is 1 hour', () => {
      const timeframe = '1h'

      describe('When the date is 1 second after the start of the timeframe', () => {
        it('should return the start of the timeframe', () => {
          const date = new Date('2000-01-01T00:00:01.000Z')
          const expected = new Date('2000-01-01T00:00:00.000Z')
          const result = start(timeframe, date)
          expect(result).toStrictEqual(O.some(expected))
        })
      })

      describe('When the date is 1 second before the start of the next timeframe', () => {
        it('should return the start of the timeframe', () => {
          const date = new Date('2000-01-01T00:59:59.000Z')
          const expected = new Date('2000-01-01T00:00:00.000Z')
          const result = start(timeframe, date)
          expect(result).toStrictEqual(O.some(expected))
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
          const expected = O.some(3599000)
          const result = millisecondsUntilNextTimeframe(timeframe, date)
          expect(result).toStrictEqual(expected)
        })
      })

      describe('When the date is 1 second before the start of the next timeframe', () => {
        it('should return the milliseconds until the next timeframe', () => {
          const date = new Date('2000-01-01T00:59:59.000Z')
          const expected = O.some(1000)
          const result = millisecondsUntilNextTimeframe(timeframe, date)
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
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
          ]
          const result = between(timeframe, startDate, endDate)
          expect(result).toStrictEqual(O.some(expected))
        })
      })

      describe('When the end date is 1 hour after the start of the timeframe', () => {
        it('should generate a range of dates correctly', () => {
          const startDate = new Date('2000-01-01T00:00:00.000Z')
          const endDate = new Date('2000-01-01T01:00:00.000Z')
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
          ]
          const result = between(timeframe, startDate, endDate)
          expect(result).toStrictEqual(O.some(expected))
        })
      })

      describe('When the end date is 2 hours after the start of the timeframe', () => {
        it('should generate a range of dates correctly', () => {
          const startDate = new Date('2000-01-01T00:00:00.000Z')
          const endDate = new Date('2000-01-01T02:00:00.000Z')
          const expected = [
            new Date('2000-01-01T00:00:00.000Z'),
            new Date('2000-01-01T01:00:00.000Z'),
          ]
          const result = between(timeframe, startDate, endDate)
          expect(result).toStrictEqual(O.some(expected))
        })
      })
    })
  })
})

