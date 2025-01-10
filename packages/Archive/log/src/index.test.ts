import { afterEach, describe, expect, it, vi } from 'vitest'

import { Logger } from '.'

describe('Logger', () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  describe('When instantiated', () => {
    describe('When default constructor is used', () => {
      const logger = new Logger()

      it('should use the default format "pretty"', () => {
        expect(logger.format).toBe('pretty')
      })

      it('should use the default level "debug"', () => {
        expect(logger.level).toBe('debug')
      })
    })

    describe('When constructor is used with custom format and level', () => {
      const logger = new Logger('json', 'info')

      it('should use the specified format', () => {
        expect(logger.format).toBe('json')
      })

      it('should use the specified level', () => {
        expect(logger.level).toBe('info')
      })
    })
  })

  describe('silly', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.silly('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })
  })

  describe('trace', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.trace('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })

    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'trace')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.trace('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })

  describe('debug', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.debug('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })

    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'debug')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.debug('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })

  describe('info', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.info('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })

    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'info')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.info('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })

  describe('warn', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.warn('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })

    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'warn')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'log')

        logger.warn('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })

  describe('error', () => {
    describe('When log level is higher than the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should not log the message', () => {
        const logSpy = vi.spyOn(console, 'error')

        logger.error('test')

        expect(logSpy).toHaveBeenCalledTimes(0)
      })
    })

    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'error')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'error')

        logger.error('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })

  describe('fatal', () => {
    describe('When log level is less than or equal to the set level', () => {
      const logger = new Logger('pretty', 'fatal')

      it('should log the message', () => {
        const logSpy = vi.spyOn(console, 'error')

        logger.fatal('test')

        expect(logSpy).toHaveBeenCalledOnce()
      })
    })
  })
})
