import { Array as A, Equal, Effect as Fx, Number as N, Option, pipe } from 'effect'

/**
 * Create a new array with a section sliced out based on the given start and end indices.
 *
 * @param start - The index at which to start the slice operation (inclusive).
 *                If omitted, defaults to the beginning of the array.
 * @param end - The index at which to end the slice operation (exclusive).
 *              If omitted, defaults to the end of the array.
 * @returns A function that takes an array of numbers and returns an Effect containing
 *          the sliced section of the array or undefined if the start and end are out of bounds.
 */
export const sliceE = (start: number | undefined, end: number | undefined) =>
  (as: Array<number>) =>
    pipe(
      as.slice(start, end),
      Fx.fromNullable,
    )

/**
 * Find all indexes of a specified item in an array.
 *
 * @param item - The item to find in the array.
 * @param array - The array to search for the item.
 * @returns An array of indexes where the item is found in the array.
 *          If the item is not found, returns an empty array.
 */
export const allIndexesOf = <T>(item: T, array: Array<T>): Array<number> =>
  pipe(
    array,
    A.filterMap((value, index) =>
      Equal.equals(value, item) ? Option.some(index) : Option.none()),
  )

/**
 * Calculate the average of an array of numbers.
 *
 * @param values - An array of numbers to calculate the average of.
 * @returns The average of the numbers in the array.
 */
export const average = (values: Array<number>): Option.Option<number> =>
  pipe(
    N.sumAll(values),
    N.divide(values.length),
  )
