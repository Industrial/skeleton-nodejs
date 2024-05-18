import { Schema } from '@effect/schema'
import { Either, Option } from 'effect'

export const factorial = (n: number): number =>
  n === 0 ? 1 : n * factorial(n - 1)

export const factorialByIteration = (maxCount = 0, counter = 0, product = 0): number =>
  counter > maxCount ? product : factorialByIteration(maxCount, counter + 1, counter * product)

export const safeDivide = (a: number, b: number): Option.Option<number> =>
  b === 0
    ? Option.none()
    : Option.some(a / b)

export const getRandomNumber = (minimum: number, maximum: number): number =>
  Math.floor(Math.random() * (maximum - minimum) + minimum)

export const decimalPlaces = (x: number): number => {
  const [, d] = x.toPrecision().split('.')
  return d ? d.length : 0
}

export const scientificDecimalPlaces = (x: number): number =>
  Math.abs(Number(String(x).split('e')[1]))

export const isScientificNotation = (x: number): boolean =>
  Boolean(/e/u.exec(String(x)))

export const isInteger = Schema.is(Schema.Int)

export const getPrecision = (a: number): Either.Either<number, string> =>
  isInteger(a)
    ? Either.left('Not an integer')
    : Either.right(isScientificNotation(a)
      ? scientificDecimalPlaces(a)
      : decimalPlaces(a))
