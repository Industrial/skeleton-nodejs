import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

export const sequenceArrayWritable = <T>(as: Array<E.Either<Error, T>>): E.Either<Error, Array<T>> =>
  pipe(as,
    E.sequenceArray,
    E.map((a) => a as Array<T>))
