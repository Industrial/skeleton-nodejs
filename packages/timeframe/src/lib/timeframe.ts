import { convertTime } from '@code9/date'
import { fromOption } from '@code9/effect'
import { Effect as Fx, pipe } from 'effect'

export type TimeframeUnit = 'd' | 'h' | 'm' | 's'
export type Timeframe = `${number}${TimeframeUnit}`

/**
 * Extracts the unit from a given timeframe.
 * @param timeframe - A string representing the timeframe (e.g., '5d' for 5 days).
 * @returns The unit of the timeframe (e.g., 'd').
 */
export const unitOf = (timeframe: Timeframe): TimeframeUnit =>
  timeframe.slice(-1) as TimeframeUnit

/**
 * Extracts the numeric value from a given timeframe.
 * @param timeframe - A string representing the timeframe (e.g., '5d' for 5 days).
 * @returns The numeric value of the timeframe (e.g., 5).
 */
export const valueOf = (timeframe: Timeframe): number =>
  parseInt(timeframe.slice(0, -1), 10)

/**
 * Creates a timeframe from a unit and value.
 * @param unit - The unit of the timeframe (e.g., 'd' for days).
 * @returns A function that takes a numeric value and returns the corresponding timeframe string.
 */
export const fromValue = (unit: TimeframeUnit) =>
  (value: number): Timeframe =>
    `${value}${unit}`

/**
 * Converts a given timeframe to milliseconds.
 * @param timeframe - A string representing the timeframe.
 * @returns An effect that resolves to the duration in milliseconds.
 */
export const toMs = <E, R>(timeframe: Timeframe): Fx.Effect<number, E | Error, R> =>
  pipe(
    valueOf(timeframe),
    convertTime(
      unitOf(timeframe),
      'ms',
    ),
    fromOption(() =>
      new Error(`Invalid timeframe: ${timeframe}`)),
  )

/**
 * Converts a duration in milliseconds to a given timeframe unit.
 * @param milliseconds - The duration in milliseconds.
 * @param unit - The target unit for the conversion.
 * @returns An effect that resolves to the converted timeframe.
 */
export const fromMs = <E, R>(milliseconds: number, unit: TimeframeUnit): Fx.Effect<Timeframe, E | Error, R> =>
  pipe(
    milliseconds,
    convertTime('ms', unit),
    fromOption(() =>
      new Error(`Invalid timeframe unit: ${unit}`)),
    Fx.map(fromValue(unit)),
  ) as Fx.Effect<Timeframe, E | Error, R>

/**
 * Adds a specified amount of time to a given date.
 * @param timeframe - The timeframe to add (e.g., '5d').
 * @param date - The date to add the timeframe to.
 * @param amount - The multiplier for the timeframe (default is 1).
 * @returns An effect that resolves to the new date.
 */
export const add = <E, R>(
  timeframe: Timeframe,
  date: Date,
  amount = 1,
): Fx.Effect<Date, E | Error, R> =>
  pipe(
    toMs(timeframe),
    Fx.map((timeframeMs) =>
      new Date(date.valueOf() + timeframeMs * amount)),
  ) as Fx.Effect<Date, E | Error, R>

/**
 * Subtracts a specified amount of time from a given date.
 * @param timeframe - The timeframe to subtract (e.g., '5d').
 * @param date - The date to subtract the timeframe from.
 * @param amount - The multiplier for the timeframe (default is 1).
 * @returns An effect that resolves to the new date.
 */
export const subtract = <E, R>(
  timeframe: Timeframe,
  date: Date,
  amount = 1,
): Fx.Effect<Date, E | Error, R> =>
  pipe(
    toMs(timeframe),
    Fx.map((timeframeMs) =>
      new Date(date.valueOf() - timeframeMs * amount)),
  ) as Fx.Effect<Date, E | Error, R>

/**
 * Subtracts a specified number of seconds from a given date.
 * @param date - The date to subtract seconds from.
 * @param amount - The number of seconds to subtract (default is 1).
 * @returns An effect that resolves to the new date.
 */
export const subtractSeconds = <E, R>(
  date: Date,
  amount = 1,
): Fx.Effect<Date, E | Error, R> =>
  pipe(
    Fx.succeed(1000),
    Fx.map((millisecond) =>
      new Date(date.valueOf() - millisecond * amount)),
  ) as Fx.Effect<Date, E | Error, R>

/**
 * Aligns a date to the start of a given timeframe.
 * @param timeframe - The timeframe to align to (e.g., '1d' for one day).
 * @param date - The date to be aligned.
 * @returns An effect that resolves to the aligned date.
 */
export const start = <E, R>(
  timeframe: Timeframe,
  date: Date,
): Fx.Effect<Date, E | Error, R> =>
  pipe(
    toMs(timeframe),
    Fx.map((timeframeMs) =>
      new Date(date.valueOf() - (date.valueOf() % timeframeMs))),
  ) as Fx.Effect<Date, E | Error, R>

/**
 * Calculates the milliseconds until the next occurrence of a given timeframe.
 * @param timeframe - The timeframe to align to (e.g., '1d' for one day).
 * @param date - The reference date.
 * @returns An effect that resolves to the duration in milliseconds until the next timeframe.
 */
export const millisecondsUntilNextTimeframe = <E, R>(
  timeframe: Timeframe,
  date: Date,
): Fx.Effect<number, E | Error, R> =>
  pipe(
    toMs(timeframe),
    Fx.map((timeframeMs) =>
      timeframeMs - (date.valueOf() % timeframeMs)),
  ) as Fx.Effect<number, E | Error, R>

/**
 * Generates a range of dates starting from a given date to an end date,
 * with each date separated by the specified timeframe.
 * @param timeframe - The timeframe to use for generating the date range.
 * @param currentDate - The starting date of the range.
 * @param endDate - The end date of the range.
 * @param dates - An array to hold the generated dates.
 * @returns An effect that resolves to an array of generated dates.
 */
const generateDateRange = <E, R>(
  timeframe: Timeframe,
  currentDate: Date,
  endDate: Date,
  dates: Array<Date>,
): Fx.Effect<Array<Date>, E | Error, R> =>
    currentDate >= endDate
      ? Fx.succeed(dates)
      : pipe(
        add(timeframe, currentDate),
        Fx.flatMap((nextDate) =>
          generateDateRange(
            timeframe,
            nextDate,
            endDate,
            [...dates, currentDate],
          )),
      ) as Fx.Effect<Array<Date>, E | Error, R>

/**
 * Generates a range of dates between two dates separated by the specified timeframe.
 * @param timeframe - The timeframe to use for generating the date range.
 * @param startDate - The starting date of the range.
 * @param endDate - The end date of the range.
 * @returns An effect that resolves to an array of dates within the range.
 */
export const between = <E, R>(
  timeframe: Timeframe,
  startDate: Date,
  endDate: Date,
): Fx.Effect<Array<Date>, E | Error, R> =>
  pipe(
    Fx.succeed(startDate <= endDate ? startDate : null),
    Fx.flatMap((result) =>
      result
        ? generateDateRange(timeframe, startDate, endDate, [])
        : Fx.succeed(null)),
  ) as Fx.Effect<Array<Date>, E | Error, R>
