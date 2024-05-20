import { Cause, Data, Effect as E, pipe } from 'effect'

// For now, assume that the timeframe is always 1m, since we cannot pass this

// Through the Tradingview Alert.
export const TIMEFRAME = '1m'

export type Timeframe = '1d' | '1h' | '1m' | '2h' | '3m' | '4h' | '5m' | '6h' | '8h' | '12h' | '15m' | '30m'

export const timeframes: Array<Timeframe> = ['1m', '3m', '5m', '15m', '30m', '1h', '2h', '4h', '6h', '8h', '12h', '1d']

const timeframeToMsMap: Record<Timeframe, number> = {
  '1m': 60 * 1000,
  '3m': 3 * 60 * 1000,
  '5m': 5 * 60 * 1000,
  '15m': 15 * 60 * 1000,
  '30m': 30 * 60 * 1000,
  '1h': 60 * 60 * 1000,
  '2h': 2 * 60 * 60 * 1000,
  '4h': 4 * 60 * 60 * 1000,
  '6h': 6 * 60 * 60 * 1000,
  '8h': 8 * 60 * 60 * 1000,
  '12h': 12 * 60 * 60 * 1000,
  '1d': 24 * 60 * 60 * 1000,
}

export const toMs = (timeframe: Timeframe) =>
  E.fromNullable(timeframeToMsMap[timeframe])

const msToTimeframeMap: Record<number, Timeframe> = {
  [60 * 1000]: '1m',
  [3 * 60 * 1000]: '3m',
  [5 * 60 * 1000]: '5m',
  [15 * 60 * 1000]: '15m',
  [30 * 60 * 1000]: '30m',
  [60 * 60 * 1000]: '1h',
  [2 * 60 * 60 * 1000]: '2h',
  [4 * 60 * 60 * 1000]: '4h',
  [6 * 60 * 60 * 1000]: '6h',
  [8 * 60 * 60 * 1000]: '8h',
  [12 * 60 * 60 * 1000]: '12h',
  [24 * 60 * 60 * 1000]: '1d',
}

export const fromMs = (milliseconds: number) =>
  E.fromNullable(msToTimeframeMap[milliseconds])

export const add = <A extends Date, E, R>(timeframe: Timeframe, amount = 1) =>
  (dateE: E.Effect<A, E, R>): E.Effect<A, Cause.NoSuchElementException | E, R> =>
    pipe(
      toMs(timeframe),
      E.flatMap((ms) =>
        pipe(
          dateE,
          E.map((date) =>
            new Date(date.valueOf() + ms * amount) as A),
        )),
    )

export const subtract = <A extends Date, E, R>(timeframe: Timeframe, amount = 1) =>
  (dateE: E.Effect<A, E, R>): E.Effect<A, Cause.NoSuchElementException | E, R> =>
    pipe(
      toMs(timeframe),
      E.flatMap((ms) =>
        pipe(
          dateE,
          E.map((date) =>
            new Date(date.valueOf() - ms * amount) as A),
        )),
    )

export const start = <A extends Date, E, R>(timeframe: Timeframe) =>
  (dateE: E.Effect<A, E, R>): E.Effect<A, Cause.NoSuchElementException | E, R> =>
    pipe(
      toMs(timeframe),
      E.flatMap((ms) =>
        pipe(
          dateE,
          E.map((date) =>
            new Date(date.valueOf() - (date.valueOf() % ms)) as A),
        )),
    )

export const millisecondsUntilNextTimeframe = <A extends Date, E, R>(timeframe: Timeframe) =>
  (dateE: E.Effect<A, E, R>): E.Effect<number, Cause.NoSuchElementException | E, R> =>
    pipe(
      toMs(timeframe),
      E.flatMap((ms) =>
        pipe(
          dateE,
          E.map((date) =>
            ms - (date.valueOf() % ms)),
        )),
    )

export class PredicateFailedError extends Data.Error<{ message: string }> {}

const generateDates = <A extends Date, E, R>(timeframe: Timeframe, dates: Array<A> = []) =>
  (endDateE: E.Effect<A, E, R>) =>
    (startDateE: E.Effect<A, E, R>): E.Effect<Array<A>, Cause.NoSuchElementException | E, R> =>
      pipe(
        E.Do,
        E.bind('startDate', () =>
          startDateE),
        E.bind('endDate', () =>
          endDateE),
        E.flatMap(({ startDate, endDate }) =>
          startDate.valueOf() < endDate.valueOf()
            ? E.succeed(dates)
            : pipe(
              startDateE,
              add(timeframe),
              pipe(
                endDateE,
                generateDates<A, Cause.NoSuchElementException | E, R>(timeframe, [...dates, startDate]),
              ),
            )),
      )

export class DateRangeError extends Data.Error<{ message: string }> {}

const normalizeStartDate = <A extends Date, E, R>(timeframe: Timeframe) =>
  (startDateE: E.Effect<A, E, R>): E.Effect<A, Cause.NoSuchElementException | E, R> =>
    pipe(
      E.Do,
      E.bind('startDate', () =>
        startDateE),
      E.bind('startDateTimeframeStart', () =>
        start(timeframe)(startDateE)),
      E.flatMap(({ startDate, startDateTimeframeStart }) =>
        pipe(startDate.valueOf() === startDateTimeframeStart.valueOf()
          ? E.succeed(startDate)
          : pipe(
            E.succeed(startDate),
            start(timeframe),
            add(timeframe),
          )) as E.Effect<A, Cause.NoSuchElementException | E, R>),
    ) as E.Effect<A, Cause.NoSuchElementException | E, R>

export const between = <A extends Date, E, R>(timeframe: Timeframe) =>
  (startDateE: E.Effect<A, E, R>) =>
    (endDateE: E.Effect<A, E, R>) =>
      pipe(
        E.Do,
        E.bind('startDate', () =>
          startDateE),
        E.bind('endDate', () =>
          endDateE),
        E.filterOrFail(
          ({ startDate, endDate }) =>
            endDate.valueOf() < startDate.valueOf(),
          () =>
            new DateRangeError({ message: 'End date is before start date.' }),
        ),
        E.filterOrFail(
          ({ startDate, endDate }) =>
            endDate.valueOf() === startDate.valueOf(),
          () =>
            new DateRangeError({ message: 'End date is equal to start date.' }),
        ),
        E.flatMap(({ endDate }) =>
          pipe(
            startDateE,
            normalizeStartDate(timeframe),
            pipe(
              E.succeed(endDate),
              generateDates<A, Cause.NoSuchElementException | E, R>(timeframe),
            ),
          )),
      )
