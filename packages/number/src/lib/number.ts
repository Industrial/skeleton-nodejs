import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as O from 'fp-ts/Option'
import * as RE from 'fp-ts/ReaderEither'

export const factorial = (n: number): number =>
  n === 0 ? 1 : n * factorial(n - 1)

export const factorialByIteration = (maxCount = 0, counter = 0, product = 0): number =>
  counter > maxCount ? product : factorialByIteration(maxCount, counter + 1, counter * product)

export const safeDivide = (a: number, b: number): O.Option<number> =>
  (b === 0 ? O.none : O.some(a / b))

export const getRandomNumber = (minimum: number, maximum: number): number =>
  Math.floor(Math.random() * (maximum - minimum) + minimum)

export const decimalPlaces = (x: number): number => {
  const [, d] = x.toPrecision().split('.')
  return d ? d.length : 0
}

export const scientificDecimalPlaces = (x: number): number =>
  Math.abs(Number(String(x).split('e')[1]))

export const isInScientificNotation = (x: number): boolean =>
  Boolean(/e/u.exec(String(x)))

export const isIntegerE = E.fromPredicate<number, Error>(
  (a) =>
    !Number.isInteger(a),
  () =>
    new Error('Not an integer'),
)

export const getPrecision: RE.ReaderEither<number, Error, number> = (a) =>
  pipe(
    a,
    isIntegerE,
    E.map(() =>
      pipe(
        a,
        O.fromPredicate(isInScientificNotation),
        O.match(() =>
          decimalPlaces(a), scientificDecimalPlaces),
      )),
  )
