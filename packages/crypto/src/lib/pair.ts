import { Either as E, pipe } from 'effect'
import type { Opaque } from 'type-fest'

export type Base = Opaque<string, 'Base'>
export type Quote = Opaque<string, 'Base'>
export type Pair = `${Base}/${Quote}`

export const uppercaseCharactersRegex = /^[A-Z]+$/u

export const isCorrectBaseE = (a: Base): E.Either<Base, Error> =>
  a !== '' && uppercaseCharactersRegex.test(a)
    ? E.right(a)
    : E.left(
        new Error(
          'Base must not be empty and must be alphanumeric uppercase characters',
        ),
      )

export const isCorrectQuoteE = (a: Base): E.Either<Base, Error> =>
  a !== '' && uppercaseCharactersRegex.test(a)
    ? E.right(a)
    : E.left(
        new Error(
          'Quote must not be empty and must be alphanumeric uppercase characters',
        ),
      )

export const isCorrectPairFormatE = (
  a: Array<string>,
): E.Either<Array<string>, Error> =>
  a.length === 2 ? E.right(a) : E.left(new Error('Invalid pair format'))

export const createPair = (base: Base, quote: Quote): E.Either<Pair, Error> =>
  pipe(
    isCorrectBaseE(base),
    E.flatMap(() =>
      pipe(
        isCorrectQuoteE(quote),
        E.map(() => `${base as Base}/${quote as Quote}` as Pair),
      ),
    ),
  )

export const getBase = (pair: Pair): E.Either<Base, Error> =>
  pipe(
    pair.split('/'),
    isCorrectPairFormatE,
    E.map((parts) => parts[0] as Base),
  )

export const getQuote = (pair: Pair): E.Either<Quote, Error> =>
  pipe(
    pair.split('/'),
    isCorrectPairFormatE,
    E.map((parts) => parts[1] as Base),
  )
