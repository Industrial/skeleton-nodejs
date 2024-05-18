import { convertTime } from '@code9/date'
import { Option } from 'effect'
import { pipe } from 'fp-ts/function'

export type TimeframeUnit = 'd' | 'h' | 'm' | 's'
export type Timeframe = `${number}${TimeframeUnit}`

export const toMs = (timeframe: Timeframe): Option.Option<number> =>
  convertTime(
    parseInt(timeframe.slice(0, -1), 10),
      timeframe.slice(-1) as TimeframeUnit,
      'ms',
  )

export const fromMs = (milliseconds: number, unit: TimeframeUnit): Option.Option<Timeframe> =>
  pipe(
    convertTime(milliseconds, 'ms', unit),
    Option.map((value): Timeframe =>
      `${value}${unit}`),
  )

export const add = (timeframe: Timeframe, date: Date, amount = 1): Option.Option<Date> =>
  pipe(
    toMs(timeframe),
    Option.flatMap((timeframeMs) =>
      Option.some(new Date(date.valueOf() + timeframeMs * amount))),
  )

export const subtract = (timeframe: Timeframe, date: Date, amount = 1): Option.Option<Date> =>
  pipe(
    toMs(timeframe),
    Option.flatMap((timeframeMs) =>
      Option.some(new Date(date.valueOf() - timeframeMs * amount))),
  )

export const subtractSeconds = (date: Date, amount = 1): Option.Option<Date> =>
  pipe(
    Option.some(1000),
    Option.flatMap((millisecond) => {
      const newTimestamp = date.valueOf() - millisecond * amount
      return Option.some(new Date(newTimestamp))
    }),
  )

export const start = (timeframe: Timeframe, date: Date): Option.Option<Date> =>
  pipe(
    toMs(timeframe),
    Option.flatMap((timeframeMs) =>
      Option.some(new Date(date.valueOf() - (date.valueOf() % timeframeMs)))),
  )

export const millisecondsUntilNextTimeframe = (timeframe: Timeframe, date: Date): Option.Option<number> =>
  pipe(
    toMs(timeframe),
    Option.flatMap((timeframeMs) =>
      Option.some(timeframeMs - (date.valueOf() % timeframeMs))),
  )

const generateDateRange = (
  timeframe: Timeframe,
  currentDate: Date,
  endDate: Date,
  dates: Array<Date>,
): Option.Option<Array<Date>> =>
  currentDate >= endDate
    ? Option.some(dates)
    : pipe(
      add(timeframe, currentDate),
      Option.flatMap((nextDate) =>
        generateDateRange(timeframe, nextDate, endDate, [...dates, currentDate])),
    )

export const between = (timeframe: Timeframe, startDate: Date, endDate: Date): Option.Option<Array<Date>> =>
  pipe(
    startDate,
    Option.liftPredicate((sd) =>
      sd <= endDate),
    Option.flatMap(() =>
      generateDateRange(timeframe, startDate, endDate, [])),
  )
