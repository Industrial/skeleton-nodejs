/**
 * Retries a promise-returning function up to a specified number of attempts.
 *
 * @template A - The type of the resolved promise value.
 * @param fn - The promise-returning function to retry.
 * @param attempts - The number of retry attempts.
 * @param delay - The delay between attempts in milliseconds.
 * @returns A promise that resolves with the function's value, or rejects after all attempts fail.
 */
export const retry = async <A>(
  fn: () => Promise<A>,
  attempts: number,
  delay: number,
): Promise<A> => {
  let lastError: unknown
  for (let i = 0; i < attempts; i++) {
    try {
      return await fn()
    } catch (error: unknown) {
      lastError = error
      if (i < attempts - 1) {
        await new Promise((res) => setTimeout(res, delay))
      }
    }
  }
  throw lastError
}
