import { asyncMap, parallel } from '.'

/**
 * Filters an array with an asynchronous predicate function, in parallel, with a limit on concurrency.
 *
 * @template A - The type of the input elements.
 * @param as - The array of input elements.
 * @param predicate - The async function to test each element for inclusion.
 * @param run - A function that executes the deferred operations, typically in parallel, with an optional concurrency limit.
 * @param limit - An optional limit on the maximum number of concurrent operations. Defaults to the length of the input array.
 * @returns A promise that resolves to an array of elements that pass the predicate function.
 */
export const asyncFilter = async <A>(
  as: A[],
  predicate: (a: A) => Promise<boolean>,
  run: (
    deferreds: Array<() => Promise<boolean>>,
    limit?: number,
  ) => Promise<boolean[]> = parallel,
  limit: number = as.length,
): Promise<A[]> => {
  const bs = await asyncMap(as, predicate, run, limit)
  return as.filter((_, index) => bs[index])
}
