import { Array as A, Effect as E, Either, Equal, Number as N, Option, pipe } from 'effect'

export const sliceE = (start?: number | undefined, end?: number | undefined) =>
  (as: Array<number>) =>
    pipe(
      as.slice(start, end),
      E.fromNullable,
    )

/**
 * Find all indexes of a specified item in an array.
 */
export const allIndexesOf = <T>(item: T, array: Array<T>): Array<number> =>
  pipe(
    array,
    A.filterMap((value, index) =>
      Equal.equals(value, item) ? Option.some(index) : Option.none()),
  )

/**
 * Get items at specified indexes from an array.
 */
export const atIndexes = <A>(indexes: Array<number>) =>
  (array: Array<A>): Either.Either<Array<A>, string> => {
    const someOutOfBounds = pipe(
      indexes,
      A.findFirst((index) =>
        index < 0 || index > array.length - 1
          ? Option.some('Index out of bounds')
          : Option.none()),
    )
    if (Option.isSome(someOutOfBounds)) {
      return Either.left(someOutOfBounds.value)
    }
    return Either.right(pipe(
      indexes,
      A.map((index) =>
        array[index]),
    ))
  }

/**
 * Calculate the average of an array of numbers.
 */
export const average = (values: Array<number>) =>
  pipe(
    N.sumAll(values),
    N.divide(values.length),
  )
