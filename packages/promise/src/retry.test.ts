import { describe, expect, it } from 'vitest'
import { retry } from './retry.ts'

describe('retry', () => {
  describe('When the function succeeds', () => {
    it('should return the result', async () => {
      const fn = async () => {
        return 1
      }
      const result = await retry(fn, 3, 100)
      expect(result).toBe(1)
    })
  })

  describe('When the function fails', () => {
    it('should retry the function up to the specified number of times', async () => {
      const fn = async () => {
        throw new Error('Error')
      }
      await expect(retry(fn, 3, 100)).rejects.toThrow('Error')
    })
  })
})
