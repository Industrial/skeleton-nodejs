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
