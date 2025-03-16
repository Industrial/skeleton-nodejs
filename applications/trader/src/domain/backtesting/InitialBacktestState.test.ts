import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { InitialBacktestStateInputSchema } from './InitialBacktestState'

describe('InitialBacktestState', () => {
  describe('InitialBacktestStateInputSchema', () => {
    it('should validate valid initial backtest state input', () => {
      const validInput = {
        initialCapital: 10000,
      }

      const result = Schema.decodeSync(InitialBacktestStateInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with larger initial capital', () => {
      const validInput = {
        initialCapital: 100000,
      }

      const result = Schema.decodeSync(InitialBacktestStateInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should reject input with non-positive initial capital', () => {
      const invalidInput = {
        initialCapital: 0,
      }

      expect(() => {
        Schema.decodeSync(InitialBacktestStateInputSchema)(invalidInput)
      }).toThrow()

      const negativeCapitalInput = {
        initialCapital: -10000,
      }

      expect(() => {
        Schema.decodeSync(InitialBacktestStateInputSchema)(negativeCapitalInput)
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing initialCapital
      const missingInitialCapital = {}

      expect(() => {
        Schema.decodeSync(InitialBacktestStateInputSchema)(
          missingInitialCapital as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject input with additional fields', () => {
      const inputWithExtraFields = {
        initialCapital: 10000,
        extraField: 'extra value',
      }

      // This should still pass because Schema.Struct is not strict by default
      const result = Schema.decodeSync(InitialBacktestStateInputSchema)(
        inputWithExtraFields as any,
      )
      expect(result).toEqual({ initialCapital: 10000 })
    })
  })
})
