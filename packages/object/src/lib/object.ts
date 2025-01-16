import type { PrefixedString, SuffixedString } from '@code9/string'

/**
 * Represents an object where keys are prefixed strings.
 *
 * @typeParam Prefix - The prefix for the keys.
 * @typeParam Value - The type of the values.
 */
export type PrefixedKeyObject<Prefix extends string, Value> = {
  [Key in PrefixedString<Prefix>]: Value
}

/**
 * Represents an object where keys are suffixed strings.
 *
 * @typeParam Suffix - The suffix for the keys.
 * @typeParam Value - The type of the values.
 */
export type SuffixedKeyObject<Suffix extends string, Value> = {
  [Key in SuffixedString<Suffix>]: Value
}

/**
 * Extracts the values from an array of entries.
 *
 * @typeParam T - The type of the values.
 * @param entries - The array of entries.
 * @returns An array of values.
 */
export const entriesValues = <T>(entries: Array<[string, T]>): Array<T> =>
  entries.map(([, value]) => value)

/**
 * Returns the entries of an object sorted by their keys.
 *
 * @typeParam T - The type of the values.
 * @param object - The object to get entries from.
 * @returns An array of entries sorted by keys.
 */
export const sortedEntries = <T>(
  object: Record<string, T>,
): Array<[string, T]> =>
  Object.entries(object).sort((a, b) => a[0].localeCompare(b[0]))

/**
 * Creates a "bag" object by limiting the number of entries and optionally adding a new entry.
 *
 * @typeParam T - The type of the values.
 * @param size - The maximum number of entries in the resulting object.
 * @param object - The original object.
 * @param v - The optional value to add.
 * @param k - The optional key to add.
 * @returns A new object with at most `size` entries.
 */
export const bag = <T>(
  size: number,
  object: Record<string, T>,
  v?: T,
  k?: string,
): Record<string, T> => {
  const entries = sortedEntries(k && v ? { ...object, [k]: v } : object)

  // We should slice from 0, because when size > entries.length, it becomes
  // negative and removes entries from the end of the array, which is not what
  // we want.
  const sliceLength = entries.length - Math.min(size, entries.length)

  return Object.fromEntries(entries.slice(sliceLength))
}
