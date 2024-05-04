import type { ReaderTask } from 'fp-ts/ReaderTask'

export const delay: ReaderTask<number, void> = (ms) =>
  async () => {
    await new Promise<void>((resolve) => {
      setTimeout(() => {
        resolve()
      }, ms)
    })
  }

export const retryTimes = async <T>(retries: number, fn: () => Promise<T>): Promise<T> => {
  try {
    return await fn()
  } catch (error: unknown) {
    if (retries > 0) {
      return retryTimes(retries - 1, fn)
    }
    throw error
  }
}

export const retryForever = async <T>(fn: () => Promise<T>): Promise<T> => {
  try {
    return await fn()
  } catch (error: unknown) {
    return retryForever(fn)
  }
}

export const retryUntil = async <T>(predicate: (value: T) => boolean, fn: () => Promise<T>): Promise<T> => {
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
