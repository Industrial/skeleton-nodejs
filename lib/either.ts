import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

export const sequenceArrayWritable = <T>(pairs: Array<E.Either<Error, T>>): E.Either<Error, Array<T>> =>
  pipe(pairs,
    E.sequenceArray,
    E.map((p) => p as Array<T>))
