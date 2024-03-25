import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import * as RE from 'fp-ts/ReaderEither'

export const safeDivide = (a: number, b: number): O.Option<number> => {
  return b === 0 ? O.none : O.some(a / b)
}

export const getRandomNumber = (minimum: number, maximum: number): number => {
  return Math.floor(Math.random() * (maximum - minimum) + minimum)
}

export const decimalPlaces = (x: number): number => {
  const [, d] = x.toPrecision().split('.')
  return d ? d.length : 0
}

export const scientificDecimalPlaces = (x: number): number => {
  return Math.abs(Number(String(x).split('e')[1]))
}

export const isInScientificNotation = (x: number): boolean => {
  return Boolean(/e/u.exec(String(x)))
}

export const isIntegerE = E.fromPredicate<number, Error>(

  (a) => {
    return !Number.isInteger(a)
  },
  () => {
    return new Error('Not an integer')
  },
)

export const getPrecision: RE.ReaderEither<number, Error, number> = (a) => {
  return pipe(
    a,
    isIntegerE,
    E.map(() => {
      return pipe(
        a,
        O.fromPredicate(isInScientificNotation),
        O.match(() => {
          return decimalPlaces(a)
        }, scientificDecimalPlaces),
      )
    }),
  )
}
