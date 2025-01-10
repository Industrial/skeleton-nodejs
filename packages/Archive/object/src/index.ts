import { isNotNull } from '@susu/null'

export const isObject = <A>(ra: unknown): ra is Record<string, A> =>
  typeof ra === 'object' && isNotNull(ra) && !Array.isArray(ra)

export const mapKeys = <A extends Record<string, unknown>>(
  input: A,
  fn: (key: string) => string,
) =>
  Object.fromEntries(
    Object.entries(input).map(([key, value]) => [fn(key), value]),
  ) as A

/**
 * Traverse an object and convert all string values to lowercase.
 *
 * @typeParam A - The type of the input object.
 * @param obj - The object to traverse.
 * @returns - A new object with all string values converted to lowercase.
 */
// biome-ignore lint/suspicious/noExplicitAny: <explanation>
export const valuesToLowerCase = <A extends Record<string, any>>(obj: A): A => {
  return Object.fromEntries(
    Object.entries(obj).map(([key, value]) => {
      if (typeof value === 'string') {
        return [key, value.toLowerCase()]
      }

      if (isObject(value)) {
        // biome-ignore lint/suspicious/noExplicitAny: <explanation>
        return [key, valuesToLowerCase(value as Record<string, any>)]
      }

      if (Array.isArray(value)) {
        return [key, value.map((item) => valuesToLowerCase({ item }).item)]
      }

      return [key, value]
    }),
  ) as A
}
