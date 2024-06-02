import { OHLCV, Position } from '@code9/trader-core'
import { describe, expect, it } from 'bun:test'
import { Effect as Fx } from 'effect'

import { backtest, buy, calculateBuyPrices, calculateSellPrices, sell } from './backtest.ts'

describe('calculateBuyPrices function', () => {
  it('should calculate buy prices correctly', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0 }
    const actual = Fx.runSync(calculateBuyPrices(props))
    const expected = {
      base: 20 / 1.5,
      quote: 0,
    }
    expect(actual).toEqual(expected)
  })

  it('should handle zero quote correctly', () => {
    const props = { base: 30, quote: 0, price: 2, transactionCostPercentage: 0 }
    expect(() =>
      Fx.runSync(calculateBuyPrices(props)))
      .toThrowError(new Error('Quote is zero'))
  })

  it('should handle zero price correctly', () => {
    const props = { base: 15, quote: 25, price: 0, transactionCostPercentage: 0 }
    expect(() =>
      Fx.runSync(calculateBuyPrices(props)))
      .toThrowError(new Error('Price is zero'))
  })

  it('should handle transactionCostPercentage < 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: -1 }
    expect(() =>
      Fx.runSync(calculateBuyPrices(props)))
      .toThrowError(new Error('transactionCostPercentage is negative'))
  })

  it('should handle transactionCostPercentage = 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0 }
    const actual = Fx.runSync(calculateBuyPrices(props))
    const expected = {
      base: 20 / 1.5,
      quote: 0,
    }
    expect(actual).toEqual(expected)
  })

  it('should handle transactionCostPercentage > 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0.5 }
    const actual = Fx.runSync(calculateBuyPrices(props))
    const expected = {
      base: 20 / 1.5 - (0.5 / 100) * (20 / 1.5),
      quote: 0,
    }
    expect(actual).toEqual(expected)
  })
})

describe('calculateSellPrices function', () => {
  it('should calculate sell prices correctly', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0 }
    const actual = Fx.runSync(calculateSellPrices(props))
    const expected = {
      base: 0,
      quote: 10 * 1.5,
    }
    expect(actual).toEqual(expected)
  })

  it('should handle zero base correctly', () => {
    const props = { base: 0, quote: 30, price: 2, transactionCostPercentage: 0 }
    expect(() =>
      Fx.runSync(calculateSellPrices(props)))
      .toThrowError(new Error('Base is zero'))
  })

  it('should handle zero price correctly', () => {
    const props = { base: 15, quote: 25, price: 0, transactionCostPercentage: 0 }
    expect(() =>
      Fx.runSync(calculateSellPrices(props)))
      .toThrowError(new Error('Price is zero'))
  })

  it('should handle transactionCostPercentage < 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: -1 }
    expect(() =>
      Fx.runSync(calculateSellPrices(props)))
      .toThrowError(new Error('transactionCostPercentage is negative'))
  })

  it('should handle transactionCostPercentage = 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0 }
    const actual = Fx.runSync(calculateSellPrices(props))
    const expected = {
      base: 0,
      quote: 10 * 1.5,
    }
    expect(actual).toEqual(expected)
  })

  it('should handle transactionCostPercentage > 0', () => {
    const props = { base: 10, quote: 20, price: 1.5, transactionCostPercentage: 0.5 }
    const actual = Fx.runSync(calculateSellPrices(props))
    const expected = {
      base: 0,
      quote: 10 * 1.5 - (0.5 / 100) * (10 * 1.5),
    }
    expect(actual).toEqual(expected)
  })
})

describe('buy function', () => {
  it('should update backtest state correctly', () => {
    const state = Fx.succeed({
      currentPosition: Position.Hold,
      currentTrade: undefined,
      currentAmountBase: 0,
      currentAmountQuote: 1000,
      trades: [],
    })
    const bars: Array<OHLCV> = [
      { time: 0, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 1, open: 0, high: 0, low: 0, close: 1.6, volume: 0 },
      { time: 2, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
    ]
    const index = 0
    const transactionCostPercentage = 0
    const bar = bars[index]
    if (!bar) {
      throw new Error('Bar is undefined')
    }

    const actualEffect = buy({ state, bar, transactionCostPercentage })
    const actual = Fx.runSync(actualEffect)
    const expected = {
      currentPosition: Position.Buy,
      currentTrade: {
        startDate: 0,
        endDate: 0,
        startPrice: 1.5,
        endPrice: 0,
        base: (1000 / 3) * 2,
        quote: 0,
      },
      currentAmountBase: (1000 / 3) * 2,
      currentAmountQuote: 0,
      trades: [],
    }
    expect(actual).toEqual(expected)
  })
})

describe('sell function', () => {
  it('should update backtest state correctly', () => {
    const state = Fx.succeed({
      currentPosition: Position.Hold,
      currentTrade: {
        startDate: 0,
        endDate: 0,
        startPrice: 1.5,
        endPrice: 0,
        base: (1000 / 3) * 2,
        quote: 0,
      },
      currentAmountBase: (1000 / 3) * 2,
      currentAmountQuote: 0,
      trades: [],
    })
    const bars: Array<OHLCV> = [
      { time: 0, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 1, open: 0, high: 0, low: 0, close: 1.6, volume: 0 },
      { time: 2, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
    ]
    const index = 1
    const transactionCostPercentage = 0
    const bar = bars[index]
    if (!bar) {
      throw new Error('Bar is undefined')
    }

    const actualEffect = sell({ state, bar, transactionCostPercentage })
    const actual = Fx.runSync(actualEffect)
    const expected = {
      currentPosition: Position.Sell,
      currentTrade: undefined,
      currentAmountBase: 0,
      currentAmountQuote: (1000 / 3) * 2 * 1.6,
      trades: [{
        startDate: 0,
        endDate: 1,
        startPrice: 1.5,
        endPrice: 1.6,
        base: 0,
        quote: (1000 / 3) * 2 * 1.6,
      }],
    }
    expect(actual).toEqual(expected)
  })
})

describe('backtest function', () => {
  it('should correctly backtest positions with buy and sell', () => {
    const bars: Array<OHLCV> = [
      { time: 0, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 1, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 2, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
      { time: 3, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
    ]
    const positions: Array<Position> = [Position.Buy, Position.Hold, Position.Sell, Position.Hold]
    const initialQuoteAmount = 1000
    const transactionCostPercentage = 0

    const actual = Fx.runSync(backtest(bars, positions, initialQuoteAmount, transactionCostPercentage))
    const expected = [{
      startDate: 0,
      endDate: 2,
      startPrice: 1.5,
      endPrice: 1.7,
      base: 0,
      quote: (initialQuoteAmount / 3) * 2 * 1.7,
    }]
    expect(actual).toEqual(expected)
  })

  it('should perform a successful backtest', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
      { time: 3, open: 112, high: 120, low: 110, close: 118, volume: 800 },
    ]
    const positions: Array<Position> = [Position.Buy, Position.Sell, Position.Hold]
    const initialAmount = 1000

    const actual = Fx.runSync(backtest(bars, positions, initialAmount))
    const expected = [{
      startDate: 1,
      endDate: 2,
      startPrice: 105,
      endPrice: 112,
      base: 0,
      quote: (1000 / 105) * 112,
    }]
    expect(actual).toEqual(expected)
  })

  it('should handle missing bars', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
    ]
    const positions: Array<Position> = [Position.Buy, Position.Sell, Position.Hold]
    const initialAmount = 1000

    expect(() =>
      Fx.runSync(backtest(bars, positions, initialAmount)))
      .toThrowError()
  })

  it('should handle errors in backtest operations', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
      { time: 3, open: 112, high: 120, low: 110, close: 118, volume: 800 },
    ]
    const positions: Array<Position> = [Position.Sell, Position.Sell, Position.Hold]
    const initialAmount = 1000

    expect(() =>
      Fx.runSync(backtest(bars, positions, initialAmount)))
      .toThrowError(new Error('Cannot sell when currentTrade is not defined'))
  })
})
