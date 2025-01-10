/**
 * Clamps a number between a minimum and maximum value.
 *
 * @param value - The number to clamp.
 * @param min - The minimum value to clamp to.
 * @param max - The maximum value to clamp to.
 * @returns The clamped number.
 */
export const clamp = (value: number, min: number, max: number): number => {
  return Math.min(Math.max(value, min), max)
}

/**
 * Rounds a number to a specified number of decimal places.
 *
 * @param value - The number to round.
 * @param decimals - The number of decimal places.
 * @returns The rounded number.
 */
export const roundToDecimalPlace = (
  value: number,
  decimals: number,
): number => {
  const factor = 10 ** decimals
  return Math.round(value * factor) / factor
}

/**
 * Checks if a number is an integer.
 *
 * @param value - The number to check.
 * @returns `true` if the number is an integer, otherwise `false`.
 */
export const isInteger = (value: number): boolean => {
  return Number.isInteger(value)
}

/**
 * Checks if a number is a float.
 *
 * @param value - The number to check.
 * @returns `true` if the number is a float, otherwise `false`.
 */
export const isFloat = (value: number): boolean => {
  return !Number.isInteger(value) && Number.isFinite(value)
}

/**
 * Computes the factorial of a number.
 *
 * @param n - The number to compute the factorial for.
 * @returns The factorial of the number.
 */
export const factorial = (n: number): number => {
  if (n < 0) return Number.NaN
  return n === 0 ? 1 : n * factorial(n - 1)
}
