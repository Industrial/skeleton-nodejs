import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'
import { PositionStatus } from './PositionStatus'
import { ProcessSignalInputSchema } from './ProcessSignalInput'
import { TradeDirection } from './TradeDirection'

describe('ProcessSignalInput', () => {
  describe('ProcessSignalInputSchema', () => {
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
      equityCurve: [[1609459200000, 10000]],
      drawdownCurve: [[1609459200000, 0]],
      peakEquity: 10500,
    }

    const sampleSignal = {
      type: 'buy',
      timestamp: 1609459200000,
      price: 50000,
      strength: 0.8,
    }

    const sampleCandlestick = {
      timestamp: 1609459200000,
      open: 49000,
      high: 51000,
      low: 48500,
      close: 50000,
      volume: 100,
    }

    it('should validate valid process signal input', () => {
      const validInput = {
        signal: sampleSignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: sampleState,
      }

      const result = Schema.decodeSync(ProcessSignalInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with different signal types', () => {
      // Buy signal
      const buySignal = {
        ...sampleSignal,
        type: 'buy',
      }

      const buySignalInput = {
        signal: buySignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: sampleState,
      }

      const buyResult = Schema.decodeSync(ProcessSignalInputSchema)(
        buySignalInput,
      )
      expect(buyResult).toEqual(buySignalInput)

      // Sell signal
      const sellSignal = {
        ...sampleSignal,
        type: 'sell',
      }

      const sellSignalInput = {
        signal: sellSignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: sampleState,
      }

      const sellResult = Schema.decodeSync(ProcessSignalInputSchema)(
        sellSignalInput,
      )
      expect(sellResult).toEqual(sellSignalInput)

      // Exit signal
      const exitSignal = {
        ...sampleSignal,
        type: 'exit',
      }

      const exitSignalInput = {
        signal: exitSignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: sampleState,
      }

      const exitResult = Schema.decodeSync(ProcessSignalInputSchema)(
        exitSignalInput,
      )
      expect(exitResult).toEqual(exitSignalInput)
    })

    it('should validate input with empty open positions', () => {
      const stateWithNoOpenPositions = {
        ...sampleState,
        openPositions: [],
      }

      const validInput = {
        signal: sampleSignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: stateWithNoOpenPositions,
      }

      const result = Schema.decodeSync(ProcessSignalInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with missing required fields', () => {
      // Missing signal
      const missingSignal = {
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
        state: sampleState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalInputSchema)(
          missingSignal as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing candlestick
      const missingCandlestick = {
        signal: sampleSignal,
        parameters: sampleParameters,
        state: sampleState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalInputSchema)(
          missingCandlestick as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing parameters
      const missingParameters = {
        signal: sampleSignal,
        candlestick: sampleCandlestick,
        state: sampleState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalInputSchema)(
          missingParameters as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing state
      const missingState = {
        signal: sampleSignal,
        candlestick: sampleCandlestick,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalInputSchema)(
          missingState as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid signal, candlestick, parameters, or state
    // because the schema uses Schema.Any for signal and candlestick and relies on
    // BacktestStateSchema and BacktestParametersSchema for validation
  })
})
