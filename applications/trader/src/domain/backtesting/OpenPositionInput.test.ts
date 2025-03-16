import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { OpenPositionInputSchema } from './OpenPositionInput'
import { TradeDirection } from './TradeDirection'

describe('OpenPositionInput', () => {
  describe('OpenPositionInputSchema', () => {
    // Sample trade for testing
    const sampleTrade = {
      direction: TradeDirection.Buy,
      price: 50000,
      volume: 1.5,
      timestamp: 1609459200000,
      fees: 0.001,
    }

    it('should validate valid open position input with required fields', () => {
      const validInput = {
        trade: sampleTrade,
      }

      const result = Schema.decodeSync(OpenPositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate open position input with all fields including optional ones', () => {
      const validInput = {
        trade: sampleTrade,
        id: 'position-123',
        metadata: {
          strategy: 'MovingAverageCrossover',
          signal: 'buy',
        },
      }

      const result = Schema.decodeSync(OpenPositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate open position input with sell trade', () => {
      const sellTrade = {
        ...sampleTrade,
        direction: TradeDirection.Sell,
      }

      const validInput = {
        trade: sellTrade,
      }

      const result = Schema.decodeSync(OpenPositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject open position input with missing required fields', () => {
      // Missing trade
      const missingTrade = {
        id: 'position-123',
      }

      expect(() => {
        Schema.decodeSync(OpenPositionInputSchema)(
          missingTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should reject open position input with invalid trade', () => {
      // Invalid trade (missing required fields)
      const invalidTrade = {
        direction: TradeDirection.Buy,
        // Missing price, volume, timestamp, fees
      }

      const invalidInput = {
        trade: invalidTrade as any,
      }

      expect(() => {
        Schema.decodeSync(OpenPositionInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject open position input with invalid metadata', () => {
      const invalidMetadataInput = {
        trade: sampleTrade,
        metadata: 'not an object',
      }

      expect(() => {
        Schema.decodeSync(OpenPositionInputSchema)(
          invalidMetadataInput as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })
  })
})
