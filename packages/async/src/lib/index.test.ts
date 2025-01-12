import { describe, expect, mock, test } from 'bun:test'
import { Effect as Fx } from 'effect'
import { delay, retryTimes } from './index.ts'

describe('delay function', () => {
  test('should wait for the specified time', async () => {
    const delayEffect = delay(1000)

    const startTime = Date.now()
    await Fx.runPromise(delayEffect)
    const endTime = Date.now()
    const elapsedTime = endTime - startTime

    expect(elapsedTime).toBeGreaterThan(950)
    expect(elapsedTime).toBeLessThan(1050)
  })
})

describe('retryTimes function', () => {
  describe('when the operation succeeds', () => {
    test('should resolve immediately on first try', async () => {
      const successMock = mock(async () => 'Success')
      const retryEffect = retryTimes(3, successMock)
      const result = await Fx.runPromise(retryEffect)
      expect(result).toBeString()
      expect(result).toBe('Success')
      expect(successMock).toHaveBeenCalledTimes(1)
    })
  })

  // describe('when the operation fails', () => {
  //   test('should retry specified number of times before failing', async () => {
  //     let attempts = 0
  //     const maxRetries = 3

  //     try {
  //       await retryTimes(maxRetries, async () => {
  //         attempts++
  //         throw new Error('Test error')
  //       })()
  //       expect.fail('Should have thrown an error')
  //     } catch (error) {
  //       expect(attempts).toBe(maxRetries + 1) // Initial attempt + retries
  //       expect(error).toBeInstanceOf(Error)
  //       expect(error.message).toBe('Test error')
  //     }
  //   })

  //   test('should succeed if operation succeeds within retry limit', async () => {
  //     let attempts = 0
  //     const result = await retryTimes(3, async () => {
  //       attempts++
  //       if (attempts < 3) {
  //         throw new Error('Not ready yet')
  //       }
  //       return 'Finally succeeded'
  //     })()

  //     expect(attempts).toBe(3)
  //     expect(result).toBe('Finally succeeded')
  //   })
  // })

  // describe('edge cases', () => {
  //   test('should handle zero retries', async () => {
  //     let attempts = 0
  //     try {
  //       await retryTimes(0, async () => {
  //         attempts++
  //         throw new Error('Test error')
  //       })()
  //       expect.fail('Should have thrown an error')
  //     } catch (error) {
  //       expect(attempts).toBe(1) // Only initial attempt, no retries
  //       expect(error).toBeInstanceOf(Error)
  //     }
  //   })

  //   test('should handle negative retry counts as zero retries', async () => {
  //     let attempts = 0
  //     try {
  //       await retryTimes(-1, async () => {
  //         attempts++
  //         throw new Error('Test error')
  //       })()
  //       expect.fail('Should have thrown an error')
  //     } catch (error) {
  //       expect(attempts).toBe(1) // Only initial attempt, no retries
  //       expect(error).toBeInstanceOf(Error)
  //     }
  //   })

  //   test('should handle async operations with delays', async () => {
  //     let attempts = 0
  //     const start = Date.now()

  //     const result = await retryTimes(2, async () => {
  //       attempts++
  //       await delay(100)()
  //       if (attempts < 2) {
  //         throw new Error('Not ready')
  //       }
  //       return 'Success after delay'
  //     })()

  //     const duration = Date.now() - start
  //     expect(attempts).toBe(2)
  //     expect(result).toBe('Success after delay')
  //     expect(duration).toBeGreaterThan(100)
  //   })
  // })

  // describe('error handling', () => {
  //   test('should convert non-Error throws to Error objects', async () => {
  //     try {
  //       await retryTimes(0, async () => {
  //         throw 'string error' // Throwing a string instead of Error
  //       })()
  //       expect.fail('Should have thrown an error')
  //     } catch (error) {
  //       expect(error).toBeInstanceOf(Error)
  //       expect(error.message).toBe('string error')
  //     }
  //   })

  //   test('should preserve Error instances', async () => {
  //     const customError = new Error('Custom error')
  //     try {
  //       await retryTimes(0, async () => {
  //         throw customError
  //       })()
  //       expect.fail('Should have thrown an error')
  //     } catch (error) {
  //       expect(error).toBeInstanceOf(Error)
  //       expect(error.message).toBe('Custom error')
  //     }
  //   })
  // })
})
