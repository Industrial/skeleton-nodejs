import fc from 'fast-check'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import { describe, expect, test } from 'vitest'

import {
  add,
  between,
  fromMs,
  generateDateRange,
  millisecondsUntilNextTimeframe,
  start,
  subtract,
  subtractSeconds,
  Timeframe,
  TimeframeUnit, toMs,
} from './timeframe.ts'

describe('toMs', () => {
  const calculateExpected = (numberPart: number, unitPart: TimeframeUnit): number | undefined => {
    if (isNaN(numberPart)) {
      return undefined
    }

    switch (unitPart) {
      case 'd':
        return numberPart * 24 * 60 * 60 * 1000
      case 'h':
        return numberPart * 60 * 60 * 1000
      case 'm':
        return numberPart * 60 * 1000
      case 's':
        return numberPart * 1000
      default:
        return undefined
    }
  }

  test('should convert timeframes to milliseconds', () => {
    fc.assert(fc.property(fc.integer(),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (numberPart, unitPart) => {
          const timeframe: Timeframe = `${numberPart}${unitPart}`
          const expected = calculateExpected(numberPart, unitPart)
          const result = toMs(timeframe)

          expect(result).toStrictEqual(O.some(expected))
        }),
      { verbose: 2 })
  })
})

describe('fromMs', () => {
  const calculateExpected = (milliseconds: number, unitPart: TimeframeUnit): Timeframe => {
    switch (unitPart) {
      case 'd':
        return `${milliseconds / (24 * 60 * 60 * 1000)}d` as Timeframe
      case 'h':
        return `${milliseconds / (60 * 60 * 1000)}h` as Timeframe
      case 'm':
        return `${milliseconds / (60 * 1000)}m` as Timeframe
      case 's':
        return `${milliseconds / 1000}s` as Timeframe
      default:
        return '0s' as Timeframe
    }
  }

  test('should convert milliseconds to timeframes', () => {
    fc.assert(fc.property(fc.integer(),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (milliseconds, unitPart) => {
          const expected = calculateExpected(milliseconds, unitPart)
          const result = fromMs(milliseconds, unitPart)

          expect(result).toStrictEqual(O.some(expected))
        }),
      { verbose: 2 })
  })
})

describe('add', () => {
  const calculateExpected = (date: Date, timeframe: Timeframe, amount: number): Date => {
    const timeframeMs = pipe(toMs(timeframe),
      O.getOrElse(() => 0))

    return new Date(date.valueOf() + timeframeMs * amount)
  }

  test('should add timeframes to dates correctly', () => {
    fc.assert(fc.property(fc.date(),
        fc.integer(),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        fc.integer({
          min: 1,
          max: 100,
        }),
        (date, numberPart, unitPart, amount) => {
          const timeframe: Timeframe = `${numberPart}${unitPart}`
          const expected = calculateExpected(date, timeframe, amount)
          const result = add(timeframe, date, amount)

          expect(result).toStrictEqual(O.some(expected))
        }),
      { verbose: 2 })
  })
})

describe('subtract', () => {
  const calculateExpected = (date: Date, timeframe: Timeframe, amount: number): Date => {
    const timeframeMs = pipe(toMs(timeframe),
      O.getOrElse(() => 0))

    return new Date(date.valueOf() - timeframeMs * amount)
  }

  test('should subtract timeframes from dates correctly', () => {
    fc.assert(fc.property(fc.date(),
        fc.integer(),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        fc.integer({
          min: 1,
          max: 100,
        }),
        (date, numberPart, unitPart, amount) => {
          const timeframe: Timeframe = `${numberPart}${unitPart}`
          const expected = calculateExpected(date, timeframe, amount)
          const result = subtract(timeframe, date, amount)

          expect(result).toStrictEqual(O.some(expected))
        }),
      { verbose: 2 })
  })

  test('should return the same date when subtracting 0 timeframes', () => {
    const date = new Date()
    const timeframe = '1h'
    const result = subtract(timeframe, date, 0)

    expect(result).toStrictEqual(O.some(date))
  })

  test('should handle subtracting 0 from an arbitrary date correctly', () => {
    fc.assert(fc.property(fc.date(), fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>, (date, unitPart) => {
        const timeframe = `0${unitPart}` as Timeframe
        const result = subtract(timeframe, date, 0)

        expect(result).toStrictEqual(O.some(date))
      }),
      { verbose: 2 })
  })
})

describe('subtractSeconds', () => {
  test('should subtract seconds correctly', () => {
    fc.assert(fc.property(fc.date(), fc.integer({ min: 1, max: 100 }), (date, amount) => {
        const expectedTimestamp = date.valueOf() - amount * 1000
        const expectedDate = new Date(expectedTimestamp)
        const result = subtractSeconds(date, amount)

        expect(result).toStrictEqual(O.some(expectedDate))
      }),
      { verbose: 2 })
  })
})

describe('start', () => {
  test('should calculate the start of the timeframe correctly', () => {
    fc.assert(fc.property(fc.date(),
        fc.integer({ min: 1, max: 100 }),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (date, amount, unit) => {
          const timeframe: Timeframe = `${amount}${unit}`
          const timeframeMs = pipe(toMs(timeframe),
            O.getOrElse(() => 0))
          const expectedTimestamp = date.valueOf() - (date.valueOf() % timeframeMs)
          const expectedDate = new Date(expectedTimestamp)
          const result = start(timeframe, date)

          expect(result).toStrictEqual(O.some(expectedDate))
        }),
      { verbose: 2 })
  })
})

describe('millisecondsUntilNextTimeframe', () => {
  test('should calculate milliseconds until the next timeframe', () => {
    fc.assert(fc.property(fc.date({
          noInvalidDate: true,
          min: new Date('2000'),
          max: new Date('2001'),
        }),
        fc.integer({
          min: 1,
        }),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (date, numberPart, unitPart) => {
          const timeframe: Timeframe = `${numberPart}${unitPart}`
          const expected = toMs(timeframe)
          const result = millisecondsUntilNextTimeframe(timeframe, date)
          if (!O.isSome(expected) || !O.isSome(result)) {
            throw new Error(`Incorrect`)
          }

          // expect(result.value).toBeGreaterThanOrEqual(0)
          expect(result.value).toBeLessThan(expected.value)
        }),
      { verbose: 2 })
  })
})

const generateDatesInRange = (current: Date, end: Date, timeframe: Timeframe, dates: Array<Date>): Array<Date> => {
  if (current <= end) {
    dates.push(current)
    const nextDate = pipe(add(timeframe, current),
      O.getOrElse(() => current))
    return generateDatesInRange(nextDate, end, timeframe, dates)
  }
  return dates
}

describe('generateDateRange', () => {
  test('should generate a range of dates correctly', () => {
    fc.assert(fc.property(fc.date({
          noInvalidDate: true,
          min: new Date('2000'),
          max: new Date('2001'),
        }),
        fc.date({
          noInvalidDate: true,
          min: new Date('2002'),
          max: new Date('2003'),
        }),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (startDate, endDate, unitPart) => {
          const timeframe = `1${unitPart}` as Timeframe
          const expectedDates = generateDatesInRange(startDate, endDate, timeframe, [])
          const result = generateDateRange(timeframe, startDate, endDate, [])

          expect(result).toEqual(O.some(expectedDates))
        }),
      { verbose: 2 })
  })
})

describe('between', () => {
  test('should generate dates between two dates correctly', () => {
    fc.assert(fc.property(fc.date({
          noInvalidDate: true,
          min: new Date('2000'),
          max: new Date('2001'),
        }),
        fc.date({
          noInvalidDate: true,
          min: new Date('2002'),
          max: new Date('2003'),
        }),
        fc.constantFrom('d', 'h', 'm', 's') as fc.Arbitrary<TimeframeUnit>,
        (startDate, endDate, unitPart) => {
          if (startDate > endDate) {
            return
          }

          const timeframe = `1${unitPart}` as Timeframe
          const expectedDates = generateDatesInRange(startDate, endDate, timeframe, [])
          const result = between(timeframe, startDate, endDate)

          expect(result).toEqual(O.some(expectedDates))
        }),
      { verbose: 2 })
  })
})
