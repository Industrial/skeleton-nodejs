/**
 * Reduces an array to a single value using an asynchronous reducer function.
 *
 * @template A - The type of the input elements.
 * @template B - The type of the accumulated result.
 * @param as - The array of input elements.
 * @param reducer - The async function that reduces each element into the accumulated result.
 * @param b - The initial value for the accumulator.
 * @returns A promise that resolves to the final accumulated value.
 */
export const asyncReduce = async <A, B>(
  as: A[],
  reducer: (b: B, a: A) => Promise<B>,
  b: B,
): Promise<B> => {
  let c = b
  for (const a of as) {
    c = await reducer(c, a)
  }
  return c
}
