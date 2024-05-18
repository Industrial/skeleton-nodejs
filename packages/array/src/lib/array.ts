import { Array, Either, Equal, Number, Option, pipe } from 'effect'

/**
 * Find all indexes of a specified item in an array.
 */
export const allIndexesOf = <T>(item: T, array: Array<T>): Array<number> =>
  pipe(
    array,
    Array.filterMap((value, index) =>
      Equal.equals(value, item) ? Option.some(index) : Option.none()),
  )

/**
 * Get items at specified indexes from an array.
 */
export const atIndexes = <A>(indexes: Array<number>) =>
  (array: Array<A>): Either.Either<Array<A>, string> => {
    const someOutOfBounds = pipe(
      indexes,
      Array.findFirst((index) =>
        index < 0 || index > array.length - 1
          ? Option.some('Index out of bounds')
          : Option.none()),
    )
    if (Option.isSome(someOutOfBounds)) {
      return Either.left(someOutOfBounds.value)
    }
    return Either.right(pipe(
      indexes,
      Array.map((index) =>
        array[index]),
    ))
  }

/**
 * Calculate the average of an array of numbers.
 */
export const average = (values: Array<number>): number =>
  Number.sumAll(values) / values.length
