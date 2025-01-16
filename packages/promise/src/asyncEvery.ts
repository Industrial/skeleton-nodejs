import { parallel } from './parallel.ts'

/**
 * Tests whether all elements in the array pass the asynchronous predicate
 * function, in parallel, with a limit on concurrency.
 *
 * @template A - The type of the input elements.
 * @param as - The array of input elements.
 * @param predicate - The async function to test each element.
 * @param run - A function that executes the deferred operations, typically in parallel, with an optional concurrency limit.
 * @param limit - An optional limit on the maximum number of concurrent operations. Defaults to the length of the input array.
 * @returns A promise that resolves to `true` if all elements pass the predicate function, otherwise `false`.
 */
export const asyncEvery = async <A>(
  as: A[],
  predicate: (a: A) => Promise<boolean>,
  run: (
    deferreds: Array<() => Promise<boolean>>,
    limit?: number,
  ) => Promise<boolean[]> = parallel,
  limit: number = as.length,
): Promise<boolean> => {
  const results = await run(
    as.map((a) => async () => await predicate(a)),
    limit,
  )
  return results.every(Boolean)
}
