import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { ClosePositionInputSchema } from './ClosePositionInput'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('ClosePositionInput', () => {
  describe('ClosePositionInputSchema', () => {
    // Sample data for testing
    const sampleOpenPosition = {
      id: 'position-123',
      status: PositionStatus.Open,
      direction: TradeDirection.Buy,
      entryPrice: 50000,
      size: 1.5,
      openTimestamp: 1609459200000,
      entryTrade: {
        direction: TradeDirection.Buy,
        price: 50000,
        volume: 1.5,
        timestamp: 1609459200000,
        fees: 0.001,
      },
    }

    const sampleExitTrade = {
      direction: TradeDirection.Sell,
      price: 55000,
      volume: 1.5,
      timestamp: 1609545600000,
      fees: 0.001,
    }

    it('should validate valid close position input', () => {
      const validInput = {
        position: sampleOpenPosition,
        exitTrade: sampleExitTrade,
      }

      const result = Schema.decodeSync(ClosePositionInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with closed position', () => {
      const closedPosition = {
        ...sampleOpenPosition,
        status: PositionStatus.Closed,
        exitPrice: 55000,
        closeTimestamp: 1609545600000,
        exitTrade: sampleExitTrade,
      }

      const invalidInput = {
        position: closedPosition,
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(ClosePositionInputSchema)(invalidInput)
      }).toThrow()
    })

    it('should reject input with missing required fields', () => {
      // Missing position
      const missingPosition = {
        exitTrade: sampleExitTrade,
      }

      expect(() => {
        Schema.decodeSync(ClosePositionInputSchema)(
          missingPosition as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing exitTrade
      const missingExitTrade = {
        position: sampleOpenPosition,
      }

      expect(() => {
        Schema.decodeSync(ClosePositionInputSchema)(
          missingExitTrade as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    it('should validate input with exit trade in opposite direction', () => {
      // For a buy position, the exit trade should be a sell
      const validInput = {
        position: sampleOpenPosition, // Buy position
        exitTrade: {
          ...sampleExitTrade,
          direction: TradeDirection.Sell, // Sell trade
        },
      }

      const result = Schema.decodeSync(ClosePositionInputSchema)(validInput)
      expect(result).toEqual(validInput)

      // For a sell position, the exit trade should be a buy
      const sellPosition = {
        ...sampleOpenPosition,
        direction: TradeDirection.Sell,
        entryTrade: {
          ...sampleOpenPosition.entryTrade,
          direction: TradeDirection.Sell,
        },
      }

      const validInputForSellPosition = {
        position: sellPosition,
        exitTrade: {
          ...sampleExitTrade,
          direction: TradeDirection.Buy, // Buy trade
        },
      }

      const resultForSellPosition = Schema.decodeSync(ClosePositionInputSchema)(
        validInputForSellPosition,
      )
      expect(resultForSellPosition).toEqual(validInputForSellPosition)
    })

    // Note: The schema doesn't validate that the exit trade direction is opposite
    // to the position direction, so we don't test for that here. That validation
    // would typically be done in the business logic.
  })
})
