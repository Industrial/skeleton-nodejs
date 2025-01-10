/**
 * Determines if the given value is `undefined`.
 *
 * @template A - The type of the value being checked.
 * @param a - The value to check.
 * @returns `true` if the value is `undefined`, otherwise `false`.
 */
export const isUndefined = <A>(a: A | undefined): a is undefined =>
  a === undefined

/**
 * Determines if the given value is not `undefined`.
 *
 * @template A - The type of the value being checked.
 * @param a - The value to check.
 * @returns `true` if the value is not `undefined`, otherwise `false`.
 */
export const isNotUndefined = <A>(a: A | undefined): a is A => a !== undefined
