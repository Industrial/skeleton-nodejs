import assert from 'assert'

export const isUndefined = <T>(value: T | undefined): value is undefined => typeof value === 'undefined'

export const isNotUndefined = <T>(value: T | undefined): value is T => typeof value !== 'undefined'

// Assertions cannot use arrow functions.

export function assertIsUndefined<T>(value: T | undefined): asserts value is undefined {
  assert(isUndefined(value))
}

export function assertIsNotUndefined<T>(value: T | undefined): asserts value is T {
  assert(isNotUndefined(value))
}
