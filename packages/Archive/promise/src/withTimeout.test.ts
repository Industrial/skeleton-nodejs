import { describe, expect, it } from 'vitest'
import { withTimeout } from '.'

describe('withTimeout', () => {
  describe('When the promise resolves before the timeout', () => {
    it('should resolve the promise', async () => {
      const promise = Promise.resolve('result')
      const timeout = 100
      const result = await withTimeout(promise, timeout)
      expect(result).toBe('result')
    })
  })

  describe('When the promise rejects before the timeout', () => {
    it('should reject the promise', async () => {
      const promise = Promise.reject(new Error('Error'))
      const timeout = 100
      await expect(withTimeout(promise, timeout)).rejects.toThrow('Error')
    })
  })

  describe('When the promise resolves after the timeout', () => {
    it('should reject the promise', async () => {
      const promise = new Promise<string>((resolve) => {
        setTimeout(() => {
          resolve('result')
        }, 100)
      })
      const timeout = 10
      await expect(withTimeout(promise, timeout)).rejects.toThrow(
        'Promise timed out',
      )
    })
  })

  describe('When the promise rejects after the timeout', () => {
    it('should reject the promise', async () => {
      const promise = new Promise<string>((resolve, reject) => {
        setTimeout(() => {
          reject(new Error('Error'))
        }, 100)
      })
      const timeout = 10
      await expect(withTimeout(promise, timeout)).rejects.toThrow(
        'Promise timed out',
      )
    })
  })
})
