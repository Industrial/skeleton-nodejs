import { Data, Effect as E } from 'effect'

export class PredicateError extends Data.TaggedError('PredicateError')<{
  message: string
}> {}

export const effectFromPredicate = <A, E extends PredicateError, R>(predicate: (a: A) => boolean, onFalse: () => E) =>
  (a: A): E.Effect<A, E, R> =>
    predicate(a)
      ? E.succeed(a)
      : E.fail(onFalse())
