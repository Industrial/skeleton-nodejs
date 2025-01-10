/**
 * Determines if the given value is `null`.
 *
 * @template A - The type of the value being checked.
 * @param a - The value to check.
 * @returns `true` if the value is `null`, otherwise `false`.
 */
export const isNull = <A>(a: A | null): a is null => a === null

/**
 * Determines if the given value is not `null`.
 *
 * @template A - The type of the value being checked.
 * @param a - The value to check.
 * @returns `true` if the value is not `null`, otherwise `false`.
 */
export const isNotNull = <A>(a: A | null): a is A => a !== null
