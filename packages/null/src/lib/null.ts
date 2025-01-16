import assert from 'node:assert'

/**
 * Determines if a value is null.
 *
 * @param a - The value to check.
 * @returns True if the value is null, false otherwise.
 */
export const isNull = <T>(a: T | null): a is null => a === null

/**
 * Determines if a value is not null.
 *
 * @param a - The value to check.
 * @returns True if the value is not null, false otherwise.
 */
export const isNotNull = <T>(a: T | null): a is T => a !== null

/**
 * Asserts that a value is null.
 *
 * @param value - The value to assert.
 * @throws Will throw an error if the value is not null.
 */
// Assertions cannot use arrow functions.

export function assertIsNull<T>(
  value: T | undefined,
): asserts value is undefined {
  assert(isNull(value))
}

/**
 * Asserts that a value is not null.
 *
 * @param value - The value to assert.
 * @throws Will throw an error if the value is null.
 */

export function assertIsNotNull<T>(value: T | undefined): asserts value is T {
  assert(isNotNull(value))
}
