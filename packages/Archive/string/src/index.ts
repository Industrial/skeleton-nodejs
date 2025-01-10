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

export type ReplaceSubstringProps = {
  inputString: string
  startMarker: string
  endMarker: string
  replacement: string
}

/**
 * Replaces all occurrences of a substring marked by a start and end marker
 * within the input string with a specified replacement string.
 *
 * @param inputString - The original string containing the text to be replaced.
 * @param startMarker - The marker indicating the start of the substring.
 * @param endMarker - The marker indicating the end of the substring.
 * @param replacement - The string to replace the marked substring with.
 * @returns The modified string with all specified substrings replaced.
 */
export const replaceSubstring = ({
  inputString,
  startMarker,
  endMarker,
  replacement,
}: ReplaceSubstringProps) => {
  const replaceRecursive = (str: string): string => {
    const startIndex = str.indexOf(startMarker)
    if (startIndex === -1 || (startMarker === '' && endMarker === '')) {
      return str
    }

    const endIndex = str.indexOf(endMarker, startIndex + startMarker.length)
    if (endIndex === -1) {
      return str
    }

    const before = str.slice(0, startIndex)
    const after = str.slice(endIndex + endMarker.length)
    return before + replacement + replaceRecursive(after)
  }

  return replaceRecursive(inputString)
}
