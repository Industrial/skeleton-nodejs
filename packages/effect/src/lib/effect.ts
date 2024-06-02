import { Data, Effect as E } from 'effect'

/**
 * Custom error class for predicate failure.
 * @extends {Data.TaggedError}
 */
export class PredicateError extends Data.TaggedError('PredicateError')<{
  message: string
}> {}

/**
 * Creates an effect from a predicate function.
 * @template A - The type of the input to the predicate function.
 * @template E - The type of the error that can be thrown.
 * @template R - The type of the result of the effect.
 * @param {function(A): boolean} predicate - The predicate function to evaluate the input.
 * @param {function(): E} onFalse - A function that returns an error when the predicate returns false.
 * @returns {function(A): E.Effect<A, E, R>} A function that takes an input and returns an effect.
 */
export const effectFromPredicate = <A, E extends PredicateError, R>(predicate: (a: A) => boolean, onFalse: () => E) =>
  (a: A): E.Effect<A, E, R> =>
    predicate(a)
      ? E.succeed(a)
      : E.fail(onFalse())
