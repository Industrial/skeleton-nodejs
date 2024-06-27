export const isNull = <T>(a: T | null): a is null =>
  a === null

export const isNotNull = <T>(a: T | null): a is T =>
  a !== null
