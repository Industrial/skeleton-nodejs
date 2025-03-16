import { describe, expect, it } from 'bun:test'
import { Schema } from 'effect'
import { PositionSizingMethod } from './PositionSizingMethod'
import { PositionStatus } from './PositionStatus'
import { ProcessSignalsInputSchema } from './ProcessSignalsInputSchema'
import { TradeDirection } from './TradeDirection'

describe('ProcessSignalsInputSchema', () => {
  describe('ProcessSignalsInputSchema', () => {
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

    const sampleInitialState = {
      equity: 10000,
      availableCapital: 10000,
      openPositions: [],
      closedPositions: [],
      trades: [],
      equityCurve: [[1609372800000, 10000]],
      drawdownCurve: [[1609372800000, 0]],
      peakEquity: 10000,
    }

    const sampleSignals = [
      {
        type: 'buy',
        timestamp: 1609459200000,
        price: 50000,
        strength: 0.8,
      },
      {
        type: 'sell',
        timestamp: 1609545600000,
        price: 55000,
        strength: 0.7,
      },
    ]

    const sampleCandlesticks = [
      {
        timestamp: 1609459200000,
        open: 49000,
        high: 51000,
        low: 48500,
        close: 50000,
        volume: 100,
      },
      {
        timestamp: 1609545600000,
        open: 54000,
        high: 56000,
        low: 53500,
        close: 55000,
        volume: 120,
      },
    ]

    it('should validate valid process signals input', () => {
      const validInput = {
        signals: sampleSignals,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
        initialState: sampleInitialState,
      }

      const result = Schema.decodeSync(ProcessSignalsInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty signals array', () => {
      const validInput = {
        signals: [],
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
        initialState: sampleInitialState,
      }

      const result = Schema.decodeSync(ProcessSignalsInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with empty candlesticks array', () => {
      const validInput = {
        signals: sampleSignals,
        candlesticks: [],
        parameters: sampleParameters,
        initialState: sampleInitialState,
      }

      const result = Schema.decodeSync(ProcessSignalsInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should validate input with initial state having open positions', () => {
      const stateWithOpenPositions = {
        ...sampleInitialState,
        equity: 10500,
        availableCapital: 9250,
        openPositions: [sampleOpenPosition],
        trades: [sampleTrade],
      }

      const validInput = {
        signals: sampleSignals,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
        initialState: stateWithOpenPositions,
      }

      const result = Schema.decodeSync(ProcessSignalsInputSchema)(validInput)
      expect(result).toEqual(validInput)
    })

    it('should reject input with missing required fields', () => {
      // Missing signals
      const missingSignals = {
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
        initialState: sampleInitialState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalsInputSchema)(
          missingSignals as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing candlesticks
      const missingCandlesticks = {
        signals: sampleSignals,
        parameters: sampleParameters,
        initialState: sampleInitialState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalsInputSchema)(
          missingCandlesticks as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing parameters
      const missingParameters = {
        signals: sampleSignals,
        candlesticks: sampleCandlesticks,
        initialState: sampleInitialState,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalsInputSchema)(
          missingParameters as unknown as Record<string, unknown>,
        )
      }).toThrow()

      // Missing initialState
      const missingInitialState = {
        signals: sampleSignals,
        candlesticks: sampleCandlesticks,
        parameters: sampleParameters,
      }

      expect(() => {
        Schema.decodeSync(ProcessSignalsInputSchema)(
          missingInitialState as unknown as Record<string, unknown>,
        )
      }).toThrow()
    })

    // Note: We don't test for invalid signals, candlesticks, parameters, or initialState
    // because the schema uses Schema.Array(Schema.Any) for signals and candlesticks and relies on
    // BacktestStateSchema and BacktestParametersSchema for validation
  })
})
