// For now, assume that the timeframe is always 1m, since we cannot pass this
// Through the Tradingview Alert.
export const TIMEFRAME = '1m'

export type Timeframe = '1d' | '1h' | '1m' | '2h' | '3m' | '4h' | '5m' | '6h' | '8h' | '12h' | '15m' | '30m'

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

export const between = (timeframe: Timeframe, startDate: Date, endDate: Date): Array<Date> => {
  const timeframeInMilliseconds = toMs(timeframe)
  if (endDate.valueOf() - startDate.valueOf() < timeframeInMilliseconds) {
    return []
  }
  const dates: Array<Date> = []
  let currentDate = start(timeframe, startDate)
  while (currentDate < endDate) {
    dates.push(currentDate)
    currentDate = add(timeframe, currentDate)
  }
  return dates
}
