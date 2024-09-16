import { describe, expect, test } from 'bun:test'

import { delay, retryTimes } from './index.ts'

describe('delay function tests', () => {
  test('should wait for the specified time', async () => {
    const startTime = Date.now()
    await delay(1000)().catch((error) => {
      console.error(error)
    })
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
})
