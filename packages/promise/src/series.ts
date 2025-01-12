/**
 * Resolves an array of promises in series.
 *
 * @template A - The type of the resolved promise values.
 * @param deferreds - An array of promises to await in series.
 * @returns A promise that resolves with an array of results.
 */
export const series = async <A extends unknown[]>(
  deferreds: {
    [K in keyof A]: () => Promise<A[K]>
  },
): Promise<A> => {
  type AElement = A[number]
  const results: Array<AElement> = []
  for (const deferred of deferreds) {
    results.push(await deferred())
  }
  return results as A
}
