import { convertTime } from '@code9/date'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'

export type TimeframeUnit = 'd' | 'h' | 'm' | 's'
export type Timeframe = `${number}${TimeframeUnit}`

export const toMs = (timeframe: Timeframe): O.Option<number> => {
  const numberPart = parseInt(timeframe, 10)
  const unitPart = timeframe.slice(-1) as TimeframeUnit

  if (isNaN(numberPart)) {
    return O.none
  }

  return O.some(pipe(
    convertTime(numberPart, unitPart, 'ms'),
    O.getOrElse(() => {
      return 0
    }),
  ))
}

export const fromMs = (milliseconds: number, unit: TimeframeUnit): O.Option<Timeframe> => {
  return pipe(
    convertTime(milliseconds, 'ms', unit),
    O.map((value) => {
      return `${value}${unit}`
    }),
  )
}

export const add = (timeframe: Timeframe, date: Date, amount = 1): O.Option<Date> => {
  return pipe(
    toMs(timeframe),
    O.chain((timeframeMs) => {
      return O.some(new Date(date.valueOf() + timeframeMs * amount))
    }),
  )
}

export const subtract = (timeframe: Timeframe, date: Date, amount = 1): O.Option<Date> => {
  return pipe(
    toMs(timeframe),
    O.chain((timeframeMs) => {
      return O.some(new Date(date.valueOf() - timeframeMs * amount))
    }),
  )
}

export const subtractSeconds = (date: Date, amount = 1): O.Option<Date> => {
  return pipe(
    O.some(1000),
    O.chain((millisecond) => {
      const newTimestamp = date.valueOf() - millisecond * amount
      return O.some(new Date(newTimestamp))
    }),
  )
}

export const start = (timeframe: Timeframe, date: Date): O.Option<Date> => {
  return pipe(
    toMs(timeframe),
    O.chain((timeframeMs) => {
      return O.some(new Date(date.valueOf() - (date.valueOf() % timeframeMs)))
    }),
  )
}

export const millisecondsUntilNextTimeframe = (timeframe: Timeframe, date: Date): O.Option<number> => {
  return pipe(
    toMs(timeframe),
    O.chain((timeframeMs) => {
      return O.some(timeframeMs - (date.valueOf() % timeframeMs))
    }),
  )
}

export const generateDateRange = (
  timeframe: Timeframe,
  currentDate: Date,
  endDate: Date,
  dates: Array<Date>,
): O.Option<Array<Date>> => {
  if (currentDate >= endDate) {
    return O.some(dates)
  }

  return pipe(
    add(timeframe, currentDate),
    O.chain((nextDate) => {
      return generateDateRange(timeframe, nextDate, endDate, [...dates, currentDate])
    }),
  )
}

export const between = (timeframe: Timeframe, startDate: Date, endDate: Date): O.Option<Array<Date>> => {
  return pipe(
    startDate,
    O.fromPredicate((sd) => {
      return sd <= endDate
    }),
    O.chain(() => {
      return generateDateRange(timeframe, startDate, endDate, [])
    }),
  )
}