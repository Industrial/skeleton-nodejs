import { afterEach, beforeEach, describe, expect, test, vi } from 'vitest'

import { delay, retryForever, retryTimes } from './async.ts'

describe('delay function tests', () => {
  beforeEach(() => {
    vi.useFakeTimers()
  })

  afterEach(() => {
    vi.restoreAllMocks()
  })

  test('should wait for the specified time', async () => {
    const startTime = Date.now()
    // TODO: What's this console doing here?
    delay(1000)().catch((error) => {
      console.error(error)
    })
    vi.advanceTimersByTime(1000)
    const endTime = Date.now()
    const elapsedTime = endTime - startTime

    // Allow a slight difference due to setTimeout inaccuracies
    expect(elapsedTime).toBeGreaterThan(950)
    expect(elapsedTime).toBeLessThan(1050)
  })
})

describe('retryTimes function', () => {
  test('should resolve if the function succeeds on first try', async () => {
    const result = await retryTimes(3, async () => 'Success')

    expect(result).toBe('Success')
  })

  test('should resolve if the function succeeds after retries', async () => {
    let count = 0

    const result = await retryTimes(3, async () => {
      count += 1
      if (count < 3) {
        throw new Error('Error')
      }
      return 'Success'
    })

    expect(result).toBe('Success')
    expect(count).toBe(3)
  })

  test('should reject if the function keeps failing', async () => {
    try {
      await retryTimes(3, async () => {
        throw new Error('Error')
      })
    } catch (error: unknown) {
      expect((error as Error).message).toBe('Error')
    }
  })
})

describe('retryForever function', () => {
  test('should resolve on successful attempt', async () => {
    let attempts = 0

    const result = await retryForever(async () => {
      attempts += 1
      if (attempts < 3) {
        throw new Error('Simulated error')
      }
      return 'Success'
    })

    expect(result).toBe('Success')
    expect(attempts).toBe(3)
  })
})
