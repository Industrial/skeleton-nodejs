import { convertTime } from '@code9/date'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'

export type TimeframeUnit = 'd' | 'h' | 'm' | 's'
export type Timeframe = `${number}${TimeframeUnit}`

export const toMs = (timeframe: Timeframe): O.Option<number> =>
  convertTime(
    parseInt(timeframe.slice(0, -1), 10),
      timeframe.slice(-1) as TimeframeUnit,
      'ms',
  )

export const fromMs = (milliseconds: number, unit: TimeframeUnit): O.Option<Timeframe> =>
  pipe(
    convertTime(milliseconds, 'ms', unit),
    O.map((value): Timeframe =>
      `${value}${unit}`),
  )

export const add = (timeframe: Timeframe, date: Date, amount = 1): O.Option<Date> =>
  pipe(
    toMs(timeframe),
    O.chain((timeframeMs) =>
      O.some(new Date(date.valueOf() + timeframeMs * amount))),
  )

export const subtract = (timeframe: Timeframe, date: Date, amount = 1): O.Option<Date> =>
  pipe(
    toMs(timeframe),
    O.chain((timeframeMs) =>
      O.some(new Date(date.valueOf() - timeframeMs * amount))),
  )

export const subtractSeconds = (date: Date, amount = 1): O.Option<Date> =>
  pipe(
    O.some(1000),
    O.chain((millisecond) => {
      const newTimestamp = date.valueOf() - millisecond * amount
      return O.some(new Date(newTimestamp))
    }),
  )

export const start = (timeframe: Timeframe, date: Date): O.Option<Date> =>
  pipe(
    toMs(timeframe),
    O.chain((timeframeMs) =>
      O.some(new Date(date.valueOf() - (date.valueOf() % timeframeMs)))),
  )

export const millisecondsUntilNextTimeframe = (timeframe: Timeframe, date: Date): O.Option<number> =>
  pipe(
    toMs(timeframe),
    O.chain((timeframeMs) =>
      O.some(timeframeMs - (date.valueOf() % timeframeMs))),
  )

const generateDateRange = (
  timeframe: Timeframe,
  currentDate: Date,
  endDate: Date,
  dates: Array<Date>,
): O.Option<Array<Date>> =>
  currentDate >= endDate
    ? O.some(dates)
    : pipe(
      add(timeframe, currentDate),
      O.chain((nextDate) =>
        generateDateRange(timeframe, nextDate, endDate, [...dates, currentDate])),
    )

export const between = (timeframe: Timeframe, startDate: Date, endDate: Date): O.Option<Array<Date>> =>
  pipe(
    startDate,
    O.fromPredicate((sd) =>
      sd <= endDate),
    O.chain(() =>
      generateDateRange(timeframe, startDate, endDate, [])),
  )
