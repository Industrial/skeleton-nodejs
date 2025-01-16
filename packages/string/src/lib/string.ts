/**
 * Represents a string type that starts with a specific prefix.
 *
 * @template Prefix - The prefix that the string must start with
 */
export type PrefixedString<Prefix extends string> = `${Prefix}${string}`

/**
 * Represents a string type that ends with a specific suffix.
 *
 * @template Suffix - The suffix that the string must end with
 */
export type SuffixedString<Suffix extends string> = `${string}${Suffix}`

/**
 * Returns true if the given value is a string.
 *
 * @param value
 * @returns `true` if the value is a string, otherwise `false`.
 */
export const isString = (value: unknown): value is string => {
  return typeof value === 'string'
}

/**
 * Capitalizes the first letter of a string.
 *
 * @param str - The string to capitalize.
 * @returns The string with the first letter capitalized.
 */
export const capitalize = (str: string): string => {
  if (!str) return str
  return str.charAt(0).toUpperCase() + str.slice(1)
}

/**
 * Reverses a string.
 *
 * @param str - The string to reverse.
 * @returns The reversed string.
 */
export const reverse = (str: string): string => {
  return str.split('').reverse().join('')
}

/**
 * Checks if a string contains only digits.
 *
 * @param str - The string to check.
 * @returns `true` if the string contains only digits, otherwise `false`.
 */
export const isNumeric = (str: string): boolean => {
  return /^\d+$/.test(str)
}
