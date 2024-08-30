import { describe, expect, it, spyOn } from 'bun:test'
import { Effect as Fx } from 'effect'
import { Stats } from 'fs'
import fs from 'fs/promises'

import { directoryExists, fileExists, pathExists } from './fs.ts'

describe('fs', () => {
  describe('pathExists', () => {
    describe('When an error is thrown', () => {
      it('should throw an error', () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(pathExists(filename)))
          .toThrowError(new Error('No'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path does not exist', () => {
      it('should return an effect with an error', async () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No such file or directory')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(pathExists(filename)))
          .toThrowError(new Error('No such file or directory'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path exists', () => {
      it('should return true', async () => {
        // @ts-expect-error annoying
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          ({
            isFile: () =>
              true,
          } as unknown as Stats))
        const filename = 'test.txt'
        const result = await Fx.runPromise(pathExists(filename))
        expect(result).toBeTruthy()
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
  })
  describe('fileExists', () => {
    describe('When an error is thrown', () => {
      it('should throw an error', () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(fileExists(filename)))
          .toThrowError(new Error('No'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path does not exist', () => {
      it('should return an effect with an error', async () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No such file or directory')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(fileExists(filename)))
          .toThrowError(new Error('No such file or directory'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path exists', () => {
      it('should return true', async () => {
        // @ts-expect-error annoying
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          ({
            isFile: () =>
              true,
          } as unknown as Stats))
        const filename = 'test.txt'
        const result = await Fx.runPromise(fileExists(filename))
        expect(result).toEqual(true)
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
  })
  describe('directoryExists', () => {
    describe('When an error is thrown', () => {
      it('should throw an error', () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(directoryExists(filename)))
          .toThrowError(new Error('No'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path does not exist', () => {
      it('should return an effect with an error', async () => {
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          Promise.reject(new Error('No such file or directory')))
        const filename = 'test.txt'
        expect(async () =>
          Fx.runPromise(directoryExists(filename)))
          .toThrowError(new Error('No such file or directory'))
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
    describe('When the path exists', () => {
      it('should return true', async () => {
        // @ts-expect-error annoying
        const spy = spyOn(fs, 'stat').mockImplementation(async () =>
          ({
            isDirectory: () =>
              true,
          } as unknown as Stats))
        const filename = 'test.txt'
        const result = await Fx.runPromise(directoryExists(filename))
        expect(result).toEqual(true)
        expect(spy).toHaveBeenCalledWith(filename)
        expect(spy).toHaveBeenCalledTimes(1)
        spy.mockRestore()
      })
    })
  })
})
