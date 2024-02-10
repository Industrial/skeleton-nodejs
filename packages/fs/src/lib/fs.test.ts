import { Stats } from 'fs'
import fs from 'fs/promises'
import { afterEach, describe, expect, it, vi } from 'vitest'

import { directoryExists, fileExists } from './fs.ts'

describe('fs', () => {
  describe('fileExists', () => {
    afterEach(() => {
      vi.restoreAllMocks()
    })

    it('should return true if the file exists', async () => {
      const filename = 'test.txt'
      const denoStateMock = vi.spyOn(fs, 'stat').mockImplementationOnce(async () =>
        Promise.resolve({ isFile: () => true } as Stats))

      const result = await fileExists(filename)
      expect(result).toBe(true)
      expect(denoStateMock).toBeCalledWith(filename)
      expect(denoStateMock).toBeCalledTimes(1)
    })

    it('should return false if the file does not exist', async () => {
      const filename = 'test.txt'
      const denoStateMock = vi.spyOn(fs, 'stat').mockImplementationOnce(async () =>
        Promise.reject(new Error('File not found')))

      const result = await fileExists(filename)
      expect(result).toBe(false)
      expect(denoStateMock).toBeCalledWith(filename)
      expect(denoStateMock).toBeCalledTimes(1)
    })
  })

  describe('directoryExists', () => {
    afterEach(() => {
      vi.restoreAllMocks()
    })

    it('should return true if the file exists', async () => {
      const filename = 'test.txt'
      const denoStateMock = vi.spyOn(fs, 'stat').mockImplementationOnce(async () =>
        Promise.resolve({ isDirectory: () => true } as Stats))

      const result = await directoryExists(filename)
      expect(result).toBe(true)
      expect(denoStateMock).toBeCalledWith(filename)
      expect(denoStateMock).toBeCalledTimes(1)
    })

    it('should return false if the file does not exist', async () => {
      const filename = 'test.txt'
      const denoStateMock = vi.spyOn(fs, 'stat').mockImplementationOnce(async () =>
        Promise.reject(new Error('File not found')))

      const result = await directoryExists(filename)
      expect(result).toBe(false)
      expect(denoStateMock).toBeCalledWith(filename)
      expect(denoStateMock).toBeCalledTimes(1)
    })
  })
})
