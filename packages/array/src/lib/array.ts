import * as A from 'fp-ts/Array'
import * as E from 'fp-ts/Either'
import { identity, pipe } from 'fp-ts/function'
import * as N from 'fp-ts/number'
import * as TE from 'fp-ts/TaskEither'

/**
 * Find all indexes of a specified item in an array.
 *
 * @param {T} item - The item to search for in the array.
 * @param {Array<T>} array - The array to search within.
 * @returns {Array<number>} An array containing the indexes of the specified item.
 */
export const allIndexesOf = <T>(item: T, array: Array<T>): Array<number> =>
  pipe(
    array,
    A.chainWithIndex((index, value) =>
      (value === item ? [index] : [])),
  )

/**
 * Get items at specified indexes from an array.
 *
 * @param {Array<number>} indexes - An array of indexes to retrieve items from.
 * @param {Array<T>} array - The array to retrieve items from.
 * @returns {E.Either<Error, ReadonlyArray<T>>} Either containing a readonly
 *                                              array with items at the
 *                                              specified indexes or an error if
 *                                              an index is out of bounds.
 */
export const atIndexes = <T>(indexes: Array<number>, array: Array<T>): E.Either<Error, ReadonlyArray<T>> =>
  pipe(
    indexes,
    E.traverseArray((index) =>
      index >= 0 && index < array.length ? E.right(array[index]) : E.left(new Error(`Index out of bounds: ${index}`))),
  )

/**
 * Calculate the average of an array of numbers.
 *
 * @param {Array<number>} values - An array of numbers to calculate the average from.
 * @returns {number} The average of the numbers in the array.
 */
export const average = (values: Array<number>): number =>
  pipe(values, A.foldMap(N.MonoidSum)(identity), (total) =>
    total / values.length)

export const firstElement = <X>([a]: Array<X>): X | undefined =>
  a

export const fromAsyncTE = <T>(iter: ArrayLike<T> | AsyncIterable<T> | Iterable<T>): TE.TaskEither<Error, Array<T>> =>
  TE.tryCatch(async () =>
    await Array.fromAsync(iter), E.toError)
