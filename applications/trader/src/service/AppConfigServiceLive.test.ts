import { type Mock, beforeEach, describe, expect, it, spyOn } from 'bun:test'
import * as fs from 'node:fs'
import { Effect } from 'effect'
import { isConfigError } from 'effect/ConfigError'
import { InvalidAppConfigError } from '../domain/config/AppConfig'
import { ConfigLoadError } from './AppConfigService'
import { loadConfig } from './AppConfigServiceLive'

describe('AppConfigServiceLive', () => {
  let readFileSyncSpy: Mock<typeof fs.readFileSync>

  beforeEach(() => {
    readFileSyncSpy = spyOn(fs, 'readFileSync').mockImplementation(((
      _path,
      _options,
    ) => {
      return 'test'
    }) as typeof fs.readFileSync)
  })

  describe('When loading configuration', () => {
    describe('When loading from JSON file succeeds', () => {
      it('should return the validated AppConfig', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({
            exchange: 'binance',
            pair: 'BTC/USDT',
            timeframe: '1h',
          })
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Right')
        if (result._tag === 'Right') {
          expect(result.right).toEqual({
            exchange: 'binance',
            pair: 'BTC/USDT',
            timeframe: '1h',
          })
        }
      })
    })

    describe('When loading from environment variables succeeds', () => {
      it('should return the validated AppConfig', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({})
        }) as typeof fs.readFileSync)

        process.env.exchange = 'binance'
        process.env.pair = 'BTC/USDT'
        process.env.timeframe = '1h'

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Right')
        if (result._tag === 'Right') {
          expect(result.right).toEqual({
            exchange: 'binance',
            pair: 'BTC/USDT',
            timeframe: '1h',
          })
        }

        // biome-ignore lint/performance/noDelete: <explanation>
        delete process.env.exchange
        // biome-ignore lint/performance/noDelete: <explanation>
        delete process.env.pair
        // biome-ignore lint/performance/noDelete: <explanation>
        delete process.env.timeframe
      })
    })

    describe('When loading from both sources fails', () => {
      it('should return a ConfigError', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({})
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Left')
        if (result._tag === 'Left') {
          expect(isConfigError(result.left)).toBeTrue()
        }
      })
    })

    describe('When the JSON file is invalid', () => {
      it('should return a ConfigLoadError', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          // Invalid JSON
          return '{]'
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Left')
        if (result._tag === 'Left') {
          expect(result.left).toBeInstanceOf(ConfigLoadError)
          expect((result.left as ConfigLoadError).message).toBe(
            'Failed to load config',
          )
        }
      })
    })

    describe('When the configuration values are invalid', () => {
      it('should return an InvalidAppConfigError for invalid exchange', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({
            exchange: 'invalid',
            pair: 'BTC/USDT',
            timeframe: '1h',
          })
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Left')
        if (result._tag === 'Left') {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          expect((result.left as InvalidAppConfigError).message).toBe(
            'Invalid exchange ID',
          )
          expect((result.left as InvalidAppConfigError).field).toBe('exchange')
          expect((result.left as InvalidAppConfigError).value).toBe('invalid')
        }
      })

      it('should return an InvalidAppConfigError for invalid timeframe', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({
            exchange: 'binance',
            pair: 'BTC/USDT',
            timeframe: 'invalid',
          })
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Left')
        if (result._tag === 'Left') {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          expect((result.left as InvalidAppConfigError).message).toBe(
            'Invalid timeframe',
          )
          expect((result.left as InvalidAppConfigError).field).toBe('timeframe')
          expect((result.left as InvalidAppConfigError).value).toBe('invalid')
        }
      })

      it('should return an InvalidAppConfigError for empty pair', () => {
        readFileSyncSpy.mockImplementationOnce(((_path, _options) => {
          return JSON.stringify({
            exchange: 'binance',
            pair: '',
            timeframe: '1h',
          })
        }) as typeof fs.readFileSync)

        const result = Effect.runSync(Effect.either(loadConfig()))

        expect(readFileSyncSpy).toHaveBeenCalledWith('config.json', 'utf-8')
        expect(result._tag).toBe('Left')
        if (result._tag === 'Left') {
          expect(result.left).toBeInstanceOf(InvalidAppConfigError)
          expect((result.left as InvalidAppConfigError).message).toBe(
            'Pair cannot be empty',
          )
          expect((result.left as InvalidAppConfigError).field).toBe('pair')
          expect((result.left as InvalidAppConfigError).value).toBe('')
        }
      })
    })
  })
})
