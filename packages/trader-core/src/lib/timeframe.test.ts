import { describe, expect, it } from 'bun:test'

import { add, between, fromMs, millisecondsUntilNextTimeframe, start, subtract, subtractSeconds, Timeframe, timeframes, toMs } from './timeframe.ts'

describe('timeframe', () => {
  describe('toMs', () => {
    it('should convert 1m to 60000 milliseconds', () => {
      const timeframe = '1m'
      const result = toMs(timeframe)
      expect(result).toEqual(60000)
    })
    it('should convert 3m to 180000 milliseconds', () => {
      const timeframe = '3m'
      const result = toMs(timeframe)
      expect(result).toEqual(180000)
    })
    it('should convert 5m to 300000 milliseconds', () => {
      const timeframe = '5m'
      const result = toMs(timeframe)
      expect(result).toEqual(300000)
    })
    it('should convert 15m to 900000 milliseconds', () => {
      const timeframe = '15m'
      const result = toMs(timeframe)
      expect(result).toEqual(900000)
    })
    it('should convert 30m to 1800000 milliseconds', () => {
      const timeframe = '30m'
      const result = toMs(timeframe)
      expect(result).toEqual(1800000)
    })
    it('should convert 1h to 3600000 milliseconds', () => {
      const timeframe = '1h'
      const result = toMs(timeframe)
      expect(result).toEqual(3600000)
    })
    it('should convert 2h to 7200000 milliseconds', () => {
      const timeframe = '2h'
      const result = toMs(timeframe)
      expect(result).toEqual(7200000)
    })
    it('should convert 4h to 14400000 milliseconds', () => {
      const timeframe = '4h'
      const result = toMs(timeframe)
      expect(result).toEqual(14400000)
    })
    it('should convert 6h to 21600000 milliseconds', () => {
      const timeframe = '6h'
      const result = toMs(timeframe)
      expect(result).toEqual(21600000)
    })
    it('should convert 8h to 28800000 milliseconds', () => {
      const timeframe = '8h'
      const result = toMs(timeframe)
      expect(result).toEqual(28800000)
    })
    it('should convert 12h to 43200000 milliseconds', () => {
      const timeframe = '12h'
      const result = toMs(timeframe)
      expect(result).toEqual(43200000)
    })
    it('should convert 1d to 86400000 milliseconds', () => {
      const timeframe = '1d'
      const result = toMs(timeframe)
      expect(result).toEqual(86400000)
    })
  })
  describe('fromMs', () => {
    it('should convert 60000 milliseconds to 1m', () => {
      const milliseconds = 60000
      const result = fromMs(milliseconds)
      expect(result).toEqual('1m')
    })
    it('should convert 180000 milliseconds to 3m', () => {
      const milliseconds = 180000
      const result = fromMs(milliseconds)
      expect(result).toEqual('3m')
    })
    it('should convert 300000 milliseconds to 5m', () => {
      const milliseconds = 300000
      const result = fromMs(milliseconds)
      expect(result).toEqual('5m')
    })
    it('should convert 900000 milliseconds to 15m', () => {
      const milliseconds = 900000
      const result = fromMs(milliseconds)
      expect(result).toEqual('15m')
    })
    it('should convert 1800000 milliseconds to 30m', () => {
      const milliseconds = 1800000
      const result = fromMs(milliseconds)
      expect(result).toEqual('30m')
    })
    it('should convert 3600000 milliseconds to 1h', () => {
      const milliseconds = 3600000
      const result = fromMs(milliseconds)
      expect(result).toEqual('1h')
    })
    it('should convert 7200000 milliseconds to 2h', () => {
      const milliseconds = 7200000
      const result = fromMs(milliseconds)
      expect(result).toEqual('2h')
    })
    it('should convert 14400000 milliseconds to 4h', () => {
      const milliseconds = 14400000
      const result = fromMs(milliseconds)
      expect(result).toEqual('4h')
    })
    it('should convert 21600000 milliseconds to 6h', () => {
      const milliseconds = 21600000
      const result = fromMs(milliseconds)
      expect(result).toEqual('6h')
    })
    it('should convert 28800000 milliseconds to 8h', () => {
      const milliseconds = 28800000
      const result = fromMs(milliseconds)
      expect(result).toEqual('8h')
    })
    it('should convert 43200000 milliseconds to 12h', () => {
      const milliseconds = 43200000
      const result = fromMs(milliseconds)
      expect(result).toEqual('12h')
    })
    it('should convert 86400000 milliseconds to 1d', () => {
      const milliseconds = 86400000
      const result = fromMs(milliseconds)
      expect(result).toEqual('1d')
    })
  })
  describe('add', () => {
    it('should add 1 minute to the start of a timeframe', () => {
      const addedTimeframe = add('1m', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 00:01:00'))
    })
    it('should add 3 minutes to the start of a timeframe', () => {
      const addedTimeframe = add('3m', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 00:03:00'))
    })
    it('should add 5 minutes to the start of a timeframe', () => {
      const addedTimeframe = add('5m', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 00:05:00'))
    })
    it('should add 15 minutes to the start of a timeframe', () => {
      const addedTimeframe = add('15m', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 00:15:00'))
    })
    it('should add 30 minutes to the start of a timeframe', () => {
      const addedTimeframe = add('30m', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 00:30:00'))
    })
    it('should add 1 hour to the start of a timeframe', () => {
      const addedTimeframe = add('1h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 01:00:00'))
    })
    it('should add 2 hours to the start of a timeframe', () => {
      const addedTimeframe = add('2h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 02:00:00'))
    })
    it('should add 4 hours to the start of a timeframe', () => {
      const addedTimeframe = add('4h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 04:00:00'))
    })
    it('should add 6 hours to the start of a timeframe', () => {
      const addedTimeframe = add('6h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 06:00:00'))
    })
    it('should add 8 hours to the start of a timeframe', () => {
      const addedTimeframe = add('8h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 08:00:00'))
    })
    it('should add 12 hours to the start of a timeframe', () => {
      const addedTimeframe = add('12h', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-01 12:00:00'))
    })
    it('should add 1 day to the start of a timeframe', () => {
      const addedTimeframe = add('1d', new Date('2000-01-01 00:00:00'), 1)
      expect(addedTimeframe).toEqual(new Date('2000-01-02 00:00:00'))
    })
  })
  describe('subtract', () => {
    it('should subtract 1 minute to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('1m', new Date('2000-01-01 00:01:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 3 minutes to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('3m', new Date('2000-01-01 00:03:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 5 minutes to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('5m', new Date('2000-01-01 00:05:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 15 minutes to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('15m', new Date('2000-01-01 00:15:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 30 minutes to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('30m', new Date('2000-01-01 00:30:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 1 hour to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('1h', new Date('2000-01-01 01:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 2 hours to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('2h', new Date('2000-01-01 02:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 4 hours to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('4h', new Date('2000-01-01 04:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 6 hours to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('6h', new Date('2000-01-01 06:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 8 hours to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('8h', new Date('2000-01-01 08:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 12 hours to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('12h', new Date('2000-01-01 12:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
    it('should subtract 1 day to the start of a timeframe', () => {
      const subtractedTimeframe = subtract('1d', new Date('2000-01-02 00:00:00'), 1)
      expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
    })
  })
  describe('subtractSeconds', () => {
    describe('Given a valid date', () => {
      it('should return the correct date when subtracting 1 second', () => {
        const subtractedTimeframe = subtractSeconds(new Date('2000-01-01 00:00:01'), 1)
        expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
      })
      it('should return the correct date when subtracting 2 seconds', () => {
        const subtractedTimeframe = subtractSeconds(new Date('2000-01-01 00:00:02'), 2)
        expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
      })
      it('should return the correct date when subtracting 5 seconds', () => {
        const subtractedTimeframe = subtractSeconds(new Date('2000-01-01 00:00:05'), 5)
        expect(subtractedTimeframe).toEqual(new Date('2000-01-01 00:00:00'))
      })
    })
    describe('Given an invalid date (e.g. null or undefined)', () => {
      it('should throw an error when trying to subtract seconds from a null/undefined date', () => {
        expect(() => {
          subtractSeconds(null as unknown as Date, 5)
        }).toThrowError()
      })
    })
  })
  describe('start', () => {
    describe('When checking start time for 1m timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('1m', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('1m', new Date('2000-01-01 00:00:30'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('1m', new Date('2000-01-01 00:00:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 3m timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('3m', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('3m', new Date('2000-01-01 00:01:30'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('3m', new Date('2000-01-01 00:02:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 5m timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('5m', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('5m', new Date('2000-01-01 00:02:30'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('5m', new Date('2000-01-01 00:04:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 15m timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('15m', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('15m', new Date('2000-01-01 00:07:30'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('15m', new Date('2000-01-01 00:14:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 30m timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('30m', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('30m', new Date('2000-01-01 00:15:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('30m', new Date('2000-01-01 00:29:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 1h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('1h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('1h', new Date('2000-01-01 00:30:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('1h', new Date('2000-01-01 00:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 2h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('2h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('2h', new Date('2000-01-01 01:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('2h', new Date('2000-01-01 01:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 4h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('4h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('4h', new Date('2000-01-01 02:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('4h', new Date('2000-01-01 03:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 6h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('6h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('6h', new Date('2000-01-01 03:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('6h', new Date('2000-01-01 05:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 8h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('8h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('8h', new Date('2000-01-01 04:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('8h', new Date('2000-01-01 07:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 12h timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('12h', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('12h', new Date('2000-01-01 06:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('12h', new Date('2000-01-01 11:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('When checking start time for 1d timeframe', () => {
      describe('And the date is at the beginning of the timeframe', () => {
        it('should return the same date when the date is at the beginning of the timeframe', () => {
          const actual = start('1d', new Date('2000-01-01 00:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is in the middle of the timeframe', () => {
        it('should return the start time for the next interval', () => {
          const actual = start('1d', new Date('2000-01-01 12:00:00'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
      describe('And the date is at the end of the timeframe', () => {
        it('should return the same date when the date is at the end of the timeframe', () => {
          const actual = start('1d', new Date('2000-01-01 23:59:59'))
          const expected = new Date('2000-01-01 00:00:00')
          expect(actual).toEqual(expected)
        })
      })
    })
  })
  describe('millisecondsUntilNextTimeframe', () => {
    describe('with valid timeframe and date', () => {
      it('should return the correct number of milliseconds for 1m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1m', new Date('2000-01-01 00:00:00'))
        const expected = 60000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 3m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('3m', new Date('2000-01-01 00:00:00'))
        const expected = 180000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 5m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('5m', new Date('2000-01-01 00:00:00'))
        const expected = 300000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 15m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('15m', new Date('2000-01-01 00:00:00'))
        const expected = 900000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 30m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('30m', new Date('2000-01-01 00:00:00'))
        const expected = 1800000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 1h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1h', new Date('2000-01-01 00:00:00'))
        const expected = 3600000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 2h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('2h', new Date('2000-01-01 00:00:00'))
        const expected = 7200000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 4h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('4h', new Date('2000-01-01 00:00:00'))
        const expected = 14400000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 6h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('6h', new Date('2000-01-01 00:00:00'))
        const expected = 21600000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 8h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('8h', new Date('2000-01-01 00:00:00'))
        const expected = 28800000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 12h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('12h', new Date('2000-01-01 00:00:00'))
        const expected = 43200000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 1d timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1d', new Date('2000-01-01 00:00:00'))
        const expected = 86400000
        expect(actual).toEqual(expected)
      })
    })
    describe('with invalid timeframe', () => {
      it('should throw an error for unknown timeframe', () => {
        expect(() => {
          millisecondsUntilNextTimeframe('1w' as Timeframe, new Date('2000-01-01 00:00:00'))
        }).toThrowError()
      })
    })
    describe('with date at start of timeframe', () => {
      timeframes.forEach((timeframe) => {
        it(`should return 0 milliseconds for ${timeframe} timeframe`, () => {
          const actual = millisecondsUntilNextTimeframe(timeframe, new Date('2000-01-01 00:00:00'))
          const expected = add(timeframe, new Date('2000-01-01 00:00:00'), 1).getTime() - new Date('2000-01-01 00:00:00').getTime()
          expect(actual).toEqual(expected)
        })
      })
    })
    describe('with date at end of timeframe', () => {
      it('should return the correct number of milliseconds for 1m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1m', new Date('2000-01-01 00:00:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 3m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('3m', new Date('2000-01-01 00:02:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 5m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('5m', new Date('2000-01-01 00:04:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 15m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('15m', new Date('2000-01-01 00:14:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 30m timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('30m', new Date('2000-01-01 00:29:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 1h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1h', new Date('2000-01-01 00:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 2h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('2h', new Date('2000-01-01 01:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 4h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('4h', new Date('2000-01-01 03:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 6h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('6h', new Date('2000-01-01 05:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 8h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('8h', new Date('2000-01-01 07:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 12h timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('12h', new Date('2000-01-01 11:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
      it('should return the correct number of milliseconds for 1d timeframe', () => {
        const actual = millisecondsUntilNextTimeframe('1d', new Date('2000-01-01 23:59:59'))
        const expected = 1000
        expect(actual).toEqual(expected)
      })
    })
  })
  describe('between', () => {
    for (const timeframe of timeframes) {
      describe(`When the start date is at the beginning of the ${timeframe} timeframe`, () => {
        const startDate = new Date('2000-01-01 00:00:00')
        describe('When the end date is before the beginning of the timeframe', () => {
          const endDate = new Date(startDate.valueOf() - 1)
          it('should return an empty array', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = []
            expect(actual).toEqual(expected)
          })
        })
        describe('When the end date is at the beginning of the timeframe', () => {
          const endDate = startDate
          it('should return an empty array', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = []
            expect(actual).toEqual(expected)
          })
        })
        describe('When the end date is within the timeframe', () => {
          const endDate = new Date(add(timeframe, startDate, 1).valueOf() - 1)
          it('should return an empty array', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = []
            expect(actual).toEqual(expected)
          })
        })
        describe('When the end date is later then the timeframe', () => {
          const endDate = new Date(add(timeframe, startDate, 2).valueOf() - 1)
          it('should return an array with dates', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = [
              startDate,
              add(timeframe, startDate, 1),
            ]
            expect(actual).toEqual(expected)
          })
        })
      })
      describe('When the start date is within the timeframe', () => {
        const startDate = new Date('2000-01-01 00:00:59')
        describe('When the end date is before the beginning of the timeframe', () => {
          const endDate = new Date(start(timeframe, startDate).valueOf() - 1)
          it('should return an empty array', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = []
            expect(actual).toEqual(expected)
          })
        })
        describe('When the end date is before the start date', () => {
          const endDate = new Date(startDate.valueOf() - 1)
          it('should return an empty array', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = []
            expect(actual).toEqual(expected)
          })
        })
        describe('When the end date is later then the timeframe', () => {
          const endDate = new Date(add(timeframe, startDate, 2).valueOf() - 1)
          it('should return an array with dates', () => {
            const actual = between(timeframe, startDate, endDate)
            const expected: Array<Date> = [
              new Date(add(timeframe, start(timeframe, startDate), 1)),
              new Date(add(timeframe, start(timeframe, startDate), 2)),
            ]
            expect(actual).toEqual(expected)
          })
        })
      })
    }
  })
})

