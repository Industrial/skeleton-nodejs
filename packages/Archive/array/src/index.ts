/**
 * Extracts the element type from an array type.
 *
 * @template ArrayType - A tuple or array type.
 * @typeParam ElementType - The type of elements in the array.
 */
export type ArrayElement<ArrayType extends readonly unknown[]> =
  ArrayType extends readonly (infer ElementType)[] ? ElementType : never

/**
 * Returns the first element in the array.
 *
 * @template A - The type of elements in the array.
 * @param as - The array from which to extract the first element.
 * @returns The first element of the array.
 */
export const head = <A>(as: Array<A>) => as[0]

/**
 * Returns a new array containing all elements except the first.
 *
 * @template A - The type of elements in the array.
 * @param as - The array from which to extract all elements except the first.
 * @returns A new array with all elements of the input array except the first.
 */
export const tail = <A>(as: Array<A>) => {
  const [, ...rest] = as
  return rest
}

/**
 * Combines two arrays into an array of tuples, pairing elements by index.
 *
 * @template A - The type of elements in the first array.
 * @template B - The type of elements in the second array.
 * @param as - The first array.
 * @param bs - The second array.
 * @returns An array of tuples, each containing an element from `as` and `bs`.
 */
export const zip = <A, B>(as: Array<A>, bs: Array<B>): Array<[A, B]> =>
  as.map((k, i) => [k, bs[i]] as [A, B])

/**
 * Checks if all elements of the second array are included in the first array.
 *
 * @template A - The type of elements in the arrays.
 * @param as - The array to be searched.
 * @param bs - The array of elements to search for.
 * @returns `true` if all elements of `bs` are included in `as`, otherwise `false`.
 */
export const includesAll = <A>(as: A[], bs: A[]) =>
  bs.every((b) => as.includes(b))

/**
 * Removes duplicate elements from an array.
 *
 * @template A - The type of elements in the array.
 * @param array - The array to process.
 * @returns A new array with unique elements.
 */
export const uniqueElements = <A>(array: Array<A>): Array<A> => [
  ...new Set(array),
]

/**
 * Checks if an array is empty.
 *
 * @template A - The type of elements in the array.
 * @param array - The array to check.
 * @returns `true` if the array is empty, otherwise `false`.
 */
export const isEmpty = <A>(array: Array<A>): boolean => array.length === 0

/**
 * Splits an array into chunks of the specified size.
 *
 * @template A - The type of elements in the array.
 * @param array - The array to split.
 * @param chunkSize - The desired size of each chunk.
 * @returns An array of chunks, each containing a subarray of up to `chunkSize` elements.
 */
export const chunkArray = <A>(
  array: Array<A>,
  chunkSize: number,
): Array<Array<A>> => {
  return array.length > chunkSize
    ? [
        array.slice(0, chunkSize),
        ...chunkArray(array.slice(chunkSize), chunkSize),
      ]
    : [array]
}
