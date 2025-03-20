import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import {
  type CloseOpenPositionsInput,
  CloseOpenPositionsInputSchema,
} from './CloseOpenPositions'
import { PositionSizingMethod } from './PositionSizingMethod'
import { PositionStatus } from './PositionStatus'
import { TradeDirection } from './TradeDirection'

describe('CloseOpenPositions', () => {
  describe('CloseOpenPositionsInputSchema', () => {
    // Sample data for testing
    const sampleParameters = {
      initialCapital: 10000,
      feeRate: 0.001,
      slippageRate: 0.0005,
      positionSizingMethod: PositionSizingMethod.Fixed,
      positionSizeValue: 1,
      reinvestProfits: true,
      maxConcurrentPositions: 5,
    }

    const sampleTrade = {
      direction: TradeDirection.Buy,
      price: 50000,
      volume: 1.5,
      timestamp: 1609459200000,
      fees: 0.001,
    }

    const sampleOpenPosition = {
      id: 'position-123',
      status: PositionStatus.Open,
      direction: TradeDirection.Buy,
      entryPrice: 50000,
      size: 1.5,
      openTimestamp: 1609459200000,
      entryTrade: sampleTrade,
    }

    const sampleState = {
      equity: 10500,
      availableCapital: 9250,
      openPositions: [sampleOpenPosition],
      closedPositions: [],
      trades: [sampleTrade],
      equityCurve: [[1609459200000, 10000] as const],
      drawdownCurve: [[1609459200000, 0] as const],
      peakEquity: 10500,
    }

    const sampleCandlestick = {
      timestamp: 1609545600000,
      open: 50000,
      high: 55000,
      low: 49000,
      close: 52000,
      volume: 100,
    }

    it('should validate valid close open positions input', () => {
      const validInput = {
        state: sampleState,
        lastCandlestick: sampleCandlestick,
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(CloseOpenPositionsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty open positions', () => {
      const stateWithNoOpenPositions = {
        ...sampleState,
        openPositions: [],
      }

      const validInput = {
        state: stateWithNoOpenPositions,
        lastCandlestick: sampleCandlestick,
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(CloseOpenPositionsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should validate input with multiple open positions', () => {
      const stateWithMultipleOpenPositions = {
        ...sampleState,
        openPositions: [
          sampleOpenPosition,
          {
            ...sampleOpenPosition,
            id: 'position-456',
            entryPrice: 48000,
          },
        ],
      }

      const validInput = {
        state: stateWithMultipleOpenPositions,
        lastCandlestick: sampleCandlestick,
        parameters: sampleParameters,
      }

      const result = Schema.decodeSync(CloseOpenPositionsInputSchema)(
        validInput,
      )
      expect(result).toEqual(validInput)
    })

    it('should reject input with missing required fields', () => {
      // Missing state
      const missingState = {
        lastCandlestick: sampleCandlestick,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(CloseOpenPositionsInputSchema)(
          missingState as unknown as CloseOpenPositionsInput,
        )
      }).toThrow()

      // Missing lastCandlestick
      const missingLastCandlestick = {
        state: sampleState,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(CloseOpenPositionsInputSchema)(
          missingLastCandlestick as unknown as CloseOpenPositionsInput,
        )
      }).toThrow()

      // Missing parameters
      const missingParameters = {
        state: sampleState,
        lastCandlestick: sampleCandlestick,
      }

      expect(() => {
        Schema.decodeSync(CloseOpenPositionsInputSchema)(
          missingParameters as unknown as CloseOpenPositionsInput,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid state, lastCandlestick, or parameters
    // because the schema uses Schema.Any for lastCandlestick and relies on
    // BacktestStateSchema and BacktestParametersSchema for validation
  })
})
