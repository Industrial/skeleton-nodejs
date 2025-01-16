/**
 * Wraps a promise to ensure it resolves within a specified timeout.
 *
 * @template A - The type of the resolved promise value.
 * @param promise - The promise to wrap.
 * @param timeout - The timeout in milliseconds.
 * @returns A promise that resolves with the original promise's value or rejects if the timeout is exceeded.
 */
export const withTimeout = <A>(
  promise: Promise<A>,
  timeout: number,
): Promise<A> => {
  return new Promise<A>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error('Promise timed out')),
      timeout,
    )
    promise
      .then((value) => {
        clearTimeout(timer)
        resolve(value)
      })
      .catch((err) => {
        clearTimeout(timer)
        reject(err)
      })
  })
}
