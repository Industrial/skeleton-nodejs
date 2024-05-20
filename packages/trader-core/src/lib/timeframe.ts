import { Data, Effect } from 'effect'

// For now, assume that the timeframe is always 1m, since we cannot pass this

// Through the Tradingview Alert.
export const TIMEFRAME = '1m'

export type Timeframe = '1d' | '1h' | '1m' | '2h' | '3m' | '4h' | '5m' | '6h' | '8h' | '12h' | '15m' | '30m'

export const timeframes: Array<Timeframe> = ['1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d']

export const toMs = (timeframe: Timeframe): number => {
  switch (timeframe) {
  case '1m':
    return 60 * 1000
  case '3m':
    return 3 * toMs('1m')
  case '5m':
    return 5 * toMs('1m')
  case '15m':
    return 15 * toMs('1m')
  case '30m':
    return 30 * toMs('1m')
  case '1h':
    return 60 * toMs('1m')
  case '2h':
    return 2 * toMs('1h')
  case '4h':
    return 4 * toMs('1h')
  case '6h':
    return 6 * toMs('1h')
  case '8h':
    return 8 * toMs('1h')
  case '12h':
    return 12 * toMs('1h')
  case '1d':
    return 24 * toMs('1h')
  default:
    throw new Error(`Invalid Timeframe: ${String(timeframe)}`)
  }
}

export const fromMs = (milliseconds: number): Timeframe => {
  switch (milliseconds) {
  case 60 * 1000:
    return '1m'
  case 3 * toMs('1m'):
    return '3m'
  case 5 * toMs('1m'):
    return '5m'
  case 15 * toMs('1m'):
    return '15m'
  case 30 * toMs('1m'):
    return '30m'
  case 60 * toMs('1m'):
    return '1h'
  case 2 * toMs('1h'):
    return '2h'
  case 4 * toMs('1h'):
    return '4h'
  case 6 * toMs('1h'):
    return '6h'
  case 8 * toMs('1h'):
    return '8h'
  case 12 * toMs('1h'):
    return '12h'
  case 24 * toMs('1h'):
    return '1d'
  default:
    throw new Error(`Invalid Timeframe: ${milliseconds}`)
  }
}

export const add = (timeframe: Timeframe, date: Date, amount = 1): Date => {
  const timeframeInMilliseconds = toMs(timeframe)
  const dateTimestamp = date.valueOf()
  const newDate = new Date(dateTimestamp + timeframeInMilliseconds * amount)
  return newDate
}

export const subtract = (timeframe: Timeframe, date: Date, amount = 1): Date => {
  const timeframeInMilliseconds = toMs(timeframe)
  const dateTimestamp = date.valueOf()
  const newDate = new Date(dateTimestamp - timeframeInMilliseconds * amount)
  return newDate
}

export const subtractSeconds = (date: Date, amount = 1): Date => {
  const dateTimestamp = date.valueOf()
  const newDate = new Date(dateTimestamp - 1000 * amount)
  return newDate
}

export const start = (timeframe: Timeframe, date: Date): Date => {
  const timeframeInMilliseconds = toMs(timeframe)
  const dateTimestamp = date.valueOf()
  const newDate = new Date(dateTimestamp - (dateTimestamp % timeframeInMilliseconds))
  return newDate
}

export const millisecondsUntilNextTimeframe = (timeframe: Timeframe, date: Date): number => {
  const timeframeMilliseconds = toMs(timeframe)
  return timeframeMilliseconds - (date.valueOf() % timeframeMilliseconds)
}

const generateDates = (timeframe: Timeframe, startDate: Date, endDate: Date, dates: Array<Date> = []): Array<Date> =>
  startDate.valueOf() < endDate.valueOf()
    ? generateDates(timeframe, add(timeframe, startDate), endDate, [...dates, startDate])
    : dates

export class DateRangeError extends Data.Error<{ message: string }> {}

export const between = (timeframe: Timeframe, startDate: Date, endDate: Date): Effect.Effect<Array<Date>, DateRangeError> =>
  Effect.gen(function *betweenGenerator() {
    return endDate.valueOf() < startDate.valueOf()
      ? yield* new DateRangeError({ message: 'End date is before start date' })
      : endDate.valueOf() === startDate.valueOf()
        ? yield* new DateRangeError({ message: 'End date is the same as start date' })
        : endDate.valueOf() - startDate.valueOf() < toMs(timeframe)
          ? yield* new DateRangeError({ message: 'End date is less than one timeframe after start date' })
          : startDate.valueOf() === start(timeframe, startDate).valueOf()
            ? generateDates(timeframe, startDate, endDate)
            : generateDates(timeframe, add(timeframe, start(timeframe, startDate)), endDate)
  })
