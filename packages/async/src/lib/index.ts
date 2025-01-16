import { Effect as Fx } from 'effect'

export const delay = (ms: number) =>
  Fx.tryPromise(() => new Promise((resolve) => setTimeout(resolve, ms)))

export const retryTimes = <T>(
  retries: number,
  fn: () => Promise<T>,
): Fx.Effect<T, Error, never> =>
  Fx.tryPromise({
    try: fn,
    catch: (e) => new Error(String(e)),
  }).pipe(
    Fx.retry({
      times: retries,
    }),
  )

export const retryForever = async <T>(fn: () => Promise<T>): Promise<T> => {
  try {
    return await fn()
  } catch (error: unknown) {
    return retryForever(fn)
  }
}

export const retryUntil = async <T>(
  predicate: (value: T) => boolean,
  fn: () => Promise<T>,
): Promise<T> => {
  try {
    const value = await fn()
    if (predicate(value)) {
      return value
    }
    return retryUntil(predicate, fn)
  } catch (error: unknown) {
    return retryUntil(predicate, fn)
  }
}

export const retry = async <T>(fn: () => Promise<T>): Promise<T> =>
  retryForever(fn)
