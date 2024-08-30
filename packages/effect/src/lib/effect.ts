import { Data, Either as E, Effect as Fx, Option as O, Predicate as P } from 'effect'

/**
 * Custom error class for predicate failure.
 * @extends {Data.TaggedError}
 */
export class PredicateError extends Data.TaggedError('PredicateError')<{
  message: string
}> {}

/**
 * Creates an effect from a predicate function.
 *
 * @template A - The type of the input to the predicate function.
 * @template E - The type of the error that can be thrown.
 * @template R - The type of the result of the effect.
 * @param predicate - The predicate function to evaluate the input.
 * @param onFalse - A function that returns an error when the predicate returns false.
 * @returns A function that takes an input and returns an effect.
 */
export const fromPredicate = <A, E, R>(predicate: P.Predicate<A>, onFalse: () => E) =>
  (a: A): Fx.Effect<A, E, R> =>
    predicate(a)
      ? Fx.succeed(a)
      : Fx.fail(onFalse())

/**
 * Creates an effect from a refinement function.
 *
 * @template A - The type of the input to the refinement function.
 * @template B - The type of the narrowed output from the refinement function.
 * @template E - The type of the error that can be thrown.
 * @template R - The type of the result of the effect.
 * @param refinement - The refinement function to evaluate the input.
 * @param onFalse - A function that returns an error when the refinement returns false.
 * @returns A function that takes an input and returns an effect.
 */
export const fromRefinement = <A, B extends A, E, R>(refinement: P.Refinement<A, B>, onFalse: () => E) =>
  (a: A): Fx.Effect<B, E, R> =>
    refinement(a)
      ? Fx.succeed(a as B)
      : Fx.fail(onFalse())

/**
 * Converts an Either type to an effect.
 *
 * @template A - The type of the success value in the Either type.
 * @template E - The type of the error value in the Either type.
 * @template R - The type of the result of the effect.
 * @param e - The Either instance to convert.
 * @returns An effect that represents the conversion of the Either instance.
 */
export const fromEither = <A, E, R>(e: E.Either<A, E>): Fx.Effect<A, E, R> =>
  E.isLeft(e)
    ? Fx.fail(e.left)
    : Fx.succeed(e.right)

/**
 * Converts an Option to an Effect. If the Option is `None`, it applies the
 * provided function to produce an error, wrapping it in the specified Effect.
 *
 * @template A - The type of the value inside the Option.
 * @template E - The type of the error that the function returns.
 * @template R - The type of the Effect result.
 * @param errorFn - A curried function that returns an error of type E if the Option is `None`.
 * @param option - The Option to be converted.
 * @returns The corresponding Effect which is either a success with type A or a failure with type E.
 */
export const fromOption = <A, E, R>(onError: () => E) =>
  (o: O.Option<A>): Fx.Effect<A, E, R> =>
    O.isSome(o)
      ? Fx.succeed(o.value)
      : Fx.fail(onError())
