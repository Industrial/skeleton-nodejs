export type PrefixedString<Prefix extends string> = `${Prefix}${string}`

export type SuffixedString<Suffix extends string> = `${string}${Suffix}`
