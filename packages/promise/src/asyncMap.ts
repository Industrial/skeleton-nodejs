import { parallel } from './parallel.ts'

/**
 * Maps over an array with an asynchronous function in parallel, with a limit on concurrency.
 *
 * @template A - The type of the input elements.
 * @template B - The type of the output elements.
 * @param as - The array of input elements.
 * @param fn - The async function to apply to each element.
 * @param run - A function that executes the deferred operations, typically in parallel, with an optional concurrency limit.
 * @param limit - An optional limit on the maximum number of concurrent operations. Defaults to the length of the input array.
 * @returns A promise that resolves to an array of transformed elements.
 */
export const asyncMap = async <A, B>(
  as: A[],
  fn: (a: A) => Promise<B>,
  run: (
    deferreds: Array<() => Promise<B>>,
    limit?: number,
  ) => Promise<B[]> = parallel,
  limit: number = as.length,
): Promise<B[]> => {
  const deferreds = as.map((a) => async () => await fn(a))
  return run(deferreds, limit)
}
