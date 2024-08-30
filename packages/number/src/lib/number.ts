import { Schema } from '@effect/schema'
import { Either, Effect as Fx, Option } from 'effect'

/**
 * Computes the factorial of a number recursively.
 *
 * @param n - The number to compute the factorial of.
 * @returns The factorial of the given number.
 */
export const factorial = (n: number): number =>
  n === 0 ? 1 : n * factorial(n - 1)

/**
 * Computes the factorial of a number using iteration.
 *
 * @param maxCount - The maximum count (typically the number for which the factorial is computed).
 * @param counter - The current counter (default is 0).
 * @param product - The current product (default is 1, not 0 to correctly compute the factorial).
 * @returns The factorial of the given number.
 */
export const factorialByIteration = (maxCount = 0, counter = 0, product = 1): number =>
  counter > maxCount
    ? product
    : factorialByIteration(maxCount, counter + 1, counter * product)

/**
 * Safely divides two numbers.
 *
 * @param a - The numerator.
 * @param b - The denominator.
 * @returns An option containing the result of the division or none if the denominator is zero.
 */
export const safeDivide = (a: number, b: number): Option.Option<number> =>
  b === 0
    ? Option.none()
    : Option.some(a / b)

/**
 * Generates a random number within a specified range.
 *
 * @param minimum - The lower bound of the range.
 * @param maximum - The upper bound of the range.
 * @returns An effect that resolves to a random number within the specified range.
 */
export const getRandomNumber = (minimum: number, maximum: number): Fx.Effect<number, never> =>
  Fx.succeed(Math.floor(Math.random() * (maximum - minimum) + minimum))

/**
 * Determines the number of decimal places in a number.
 *
 * @param x - The number to determine the decimal places of.
 * @returns The number of decimal places in the number.
 */
export const decimalPlaces = (x: number): number => {
  const [, d] = x.toPrecision().split('.')
  return d ? d.length : 0
}

/**
 * Determines the number of decimal places in a number represented in scientific notation.
 *
 * @param x - The number to determine the decimal places of.
 * @returns The number of decimal places in the number's scientific notation.
 */
export const scientificDecimalPlaces = (x: number): number =>
  Math.abs(Number(String(x).split('e')[1]))

/**
 * Checks if a number is represented in scientific notation.
 *
 * @param x - The number to check.
 * @returns True if the number is in scientific notation, false otherwise.
 */
export const isScientificNotation = (x: number): boolean =>
  Boolean(/e/u.exec(String(x)))

/**
 * A schema that checks if a value is an integer.
 */
export const isInteger = Schema.is(Schema.Int)

/**
 * Gets the precision of a number.
 *
 * @param a - The number to get the precision of.
 * @returns An either containing the precision of the number if it's not an integer, or an error message if it is an integer.
 */
export const getPrecision = (a: number): Either.Either<number, string> =>
  isInteger(a)
    ? Either.left('Not an integer')
    : Either.right(isScientificNotation(a)
      ? scientificDecimalPlaces(a)
      : decimalPlaces(a))
