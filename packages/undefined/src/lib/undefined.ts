import assert from 'assert'

export type Maybe<A> = A | undefined

export const isUndefined = <T>(value: T | undefined): value is undefined =>
  typeof value === 'undefined'

export const isNotUndefined = <T>(value: T | undefined): value is T =>
  typeof value !== 'undefined'

// Assertions cannot use arrow functions.
// eslint-disable-next-line func-style
export function assertIsUndefined<T>(value: T | undefined): asserts value is undefined {
  assert(isUndefined(value))
}

// eslint-disable-next-line func-style
export function assertIsNotUndefined<T>(value: T | undefined): asserts value is T {
  assert(isNotUndefined(value))
}
