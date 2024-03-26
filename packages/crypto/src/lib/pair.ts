import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import { Opaque } from 'type-fest'

export type Base = Opaque<string, 'Base'>
export type Quote = Opaque<string, 'Base'>
export type Pair = `${Base}/${Quote}`

export const uppercaseCharactersRegex = /^[A-Z]+$/u

export const isCorrectBaseE = (base: string): E.Either<Error, Base> =>
  pipe(
    base,
    E.fromPredicate(
      (a: string) =>
        a !== '' && uppercaseCharactersRegex.test(a),
      () =>
        new Error('Base must not be empty and must be alphanumeric uppercase characters'),
    ),
  ) as E.Either<Error, Base>

export const isCorrectQuoteE = (quote: string): E.Either<Error, Quote> =>
  pipe(
    quote,
    E.fromPredicate(
      (a: string) =>
        a !== '' && uppercaseCharactersRegex.test(a),
      () =>
        new Error('Quote must not be empty and must be alphanumeric uppercase characters'),
    ),
  ) as E.Either<Error, Quote>

export const isCorrectPairFormatE = (format: Array<string>): E.Either<Error, Array<string>> =>
  pipe(
    format,
    E.fromPredicate(
      (parts) =>
        parts.length === 2,
      () =>
        new Error('Invalid pair format'),
    ),
  )

export const createPair = (base: string, quote: string): E.Either<Error, Pair> =>
  pipe(
    isCorrectBaseE(base),
    E.chain(() =>
      pipe(isCorrectQuoteE(quote))),
    E.map(() =>
      `${base as Base}/${quote as Quote}`),
  ) as E.Either<Error, Pair>

export const getBase = (pair: Pair): E.Either<Error, Base> =>
  pipe(
    pair.split('/'),
    isCorrectPairFormatE,
    E.map((parts) =>
parts[0] as Base),
  )

export const getQuote = (pair: Pair): E.Either<Error, Quote> =>
  pipe(
    pair.split('/'),
    isCorrectPairFormatE,
    E.map((parts) =>
parts[1] as Base),
  )
