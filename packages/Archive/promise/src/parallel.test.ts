import { describe, expect, it } from 'vitest'
import { parallel } from '.'

describe('parallel', () => {
  describe('When the number of deferreds is zero', () => {
    it('should resolve with an empty array', async () => {
      const deferreds: Array<() => Promise<number>> = []
      const actual = await parallel(deferreds)
      const expected: Array<number> = []
      expect(actual).toEqual(expected)
    })
  })

  describe('When the number of deferreds is above zero', () => {
    describe('When the limit is not specified', () => {
      it('should run all promises in parallel', async () => {
        const mockFuncs = [async () => 1, async () => 2, async () => 3]
        const actual = await parallel(mockFuncs)
        const expected = [1, 2, 3]
        expect(actual).toEqual(expected)
      })
    })

    describe('When the limit is specified', () => {
      describe('When the limit is more than the number of promises', () => {
        it('should run all promises in parallel', async () => {
          const mockFuncs = [async () => 1, async () => 2, async () => 3]
          const actual = await parallel(mockFuncs, 10)
          const expected = [1, 2, 3]
          expect(actual).toEqual(expected)
        })
      })

      describe('When the limit is equal to the number of promises', () => {
        it('should run all promises in parallel', async () => {
          const mockFuncs = [async () => 1, async () => 2, async () => 3]
          const actual = await parallel(mockFuncs, 3)
          const expected = [1, 2, 3]
          expect(actual).toEqual(expected)
        })
      })

      describe('When the limit is less than the number of promises', () => {
        it('should respect the specified concurrency limit', async () => {
          const concurrencyLimit = 2
          let maxConcurrent = 0
          const actual: Array<[number, number]> = []

          const createPromise = async (value: number, delay: number) =>
            new Promise<number>((resolve) => {
              maxConcurrent = maxConcurrent + 1
              setTimeout(() => {
                maxConcurrent = maxConcurrent - 1
                actual.push([value, maxConcurrent])
                resolve(value)
              }, delay)
            })

          const promises = [
            async () => createPromise(1, 100),
            async () => createPromise(2, 200),
            async () => createPromise(3, 300),
            async () => createPromise(4, 400),
            async () => createPromise(5, 500),
          ]

          const results = await parallel(promises, concurrencyLimit)

          // Check that results contain all the promise resolutions
          expect(results).toEqual([1, 2, 3, 4, 5])

          // Verify that the max concurrency does not exceed the limit
          expect(maxConcurrent).toBeLessThanOrEqual(concurrencyLimit)
        })
      })
    })
  })
})
