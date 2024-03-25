import { OHLCV, Position } from '@code9/trader-core'
import * as E from 'fp-ts/Either'
import * as RNEA from 'fp-ts/ReadonlyNonEmptyArray'
import { describe, expect, test } from 'vitest'

import { backtest, buy, calculateBuyPrices, calculateSellPrices, sell } from './backtest.ts'

describe('calculateBuyPrices function', () => {
  test('should calculate sell prices correctly', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 20 / 1.5,
        quote: 0,
      }))
  })

  test('should handle zero quote correctly', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 30,
      quote: 0,
      price: 2,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('Quote is zero')))
  })

  test('should handle zero price correctly', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 15,
      quote: 25,
      price: 0,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('Price is zero')))
  })

  test('should handle transactionCostPercentage < 0', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: -1,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('transactionCostPercentage is negative')))
  })

  test('should handle transactionCostPercentage = 0', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 20 / 1.5,
        quote: 0,
      }))
  })

  test('should handle transactionCostPercentage > 0', () => {
    // Arrange

    // Act
    const actual = calculateBuyPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0.5,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 20 / 1.5 - (0.5 / 100) * (20 / 1.5),
        quote: 0,
      }))
  })
})

describe('calculateSellPrices function', () => {
  test('should calculate sell prices correctly', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 0,
        quote: 10 * 1.5,
      }))
  })

  test('should handle zero base correctly', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 0,
      quote: 30,
      price: 2,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('Base is zero')))
  })

  test('should handle zero price correctly', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 15,
      quote: 25,
      price: 0,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('Price is zero')))
  })

  test('should handle transactionCostPercentage < 0', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: -1,
    })

    // Assert
    expect(actual).toEqual(E.left(new Error('transactionCostPercentage is negative')))
  })

  test('should handle transactionCostPercentage = 0', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 0,
        quote: 10 * 1.5,
      }))
  })

  test('should handle transactionCostPercentage > 0', () => {
    // Arrange

    // Act
    const actual = calculateSellPrices({
      base: 10,
      quote: 20,
      price: 1.5,
      transactionCostPercentage: 0.5,
    })

    // Assert
    expect(actual).toEqual(E.right({
        base: 0,
        quote: 10 * 1.5 - (0.5 / 100) * (10 * 1.5),
      }))
  })
})

describe('buy function', () => {
  test('should update backtest state correctly', () => {
    const state = E.right({
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

    const actual = buy({ state, bar, transactionCostPercentage })

    const expected = E.right({
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
    })

    expect(actual).toEqual(expected)
  })
})

describe('sell function', () => {
  test('should update backtest state correctly', () => {
    const state = E.right({
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

    const actual = sell({ state, bar, transactionCostPercentage })

    const expected = E.right({
      currentPosition: Position.Sell,
      currentTrade: undefined,
      currentAmountBase: 0,
      currentAmountQuote: (1000 / 3) * 2 * 1.6,
      trades: [
        {
          startDate: 0,
          endDate: 1,
          startPrice: 1.5,
          endPrice: 1.6,
          base: 0,
          quote: (1000 / 3) * 2 * 1.6,
        },
      ],
    })

    expect(actual).toEqual(expected)
  })
})

describe('backtest function', () => {
  test('should correctly backtest positions with buy and sell', () => {
    const bars: Array<OHLCV> = [
      { time: 0, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 1, open: 0, high: 0, low: 0, close: 1.5, volume: 0 },
      { time: 2, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
      { time: 3, open: 0, high: 0, low: 0, close: 1.7, volume: 0 },
    ]
    const positions: RNEA.ReadonlyNonEmptyArray<Position> = [Position.Buy, Position.Hold, Position.Sell, Position.Hold]
    const initialQuoteAmount = 1000
    const transactionCostPercentage = 0

    const expected = E.right([
      {
        startDate: 0,
        endDate: 2,
        startPrice: 1.5,
        endPrice: 1.7,
        base: 0,
        quote: (initialQuoteAmount / 3) * 2 * 1.7,
      },
    ])

    const actual = backtest(bars, positions, initialQuoteAmount, transactionCostPercentage)

    expect(actual).toEqual(expected)
  })

  test('should perform a successful backtest', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
      { time: 3, open: 112, high: 120, low: 110, close: 118, volume: 800 },
    ]
    const positions: RNEA.ReadonlyNonEmptyArray<Position> = [Position.Buy, Position.Sell, Position.Hold]
    const initialAmount = 1000

    const result = backtest(bars, positions, initialAmount)

    expect(E.isRight(result)).toBe(true)
    expect(result).toMatchObject(E.right([
        {
          startDate: 1,
          endDate: 2,
          startPrice: 105,
          endPrice: 112,
          base: 0,
          quote: (1000 / 105) * 112,
        },
      ]))
  })

  test('should handle missing bars', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
    ]
    const positions: RNEA.ReadonlyNonEmptyArray<Position> = [Position.Buy, Position.Sell, Position.Hold]
    const initialAmount = 1000

    const result = backtest(bars, positions, initialAmount)

    expect(E.isLeft(result)).toBe(true)
    expect(result).toMatchObject(E.left(new Error('Invalid position: Hold')))
  })

  test('should handle errors in backtest operations', () => {
    const bars = [
      { time: 1, open: 100, high: 110, low: 90, close: 105, volume: 1000 },
      { time: 2, open: 105, high: 115, low: 100, close: 112, volume: 1200 },
      { time: 3, open: 112, high: 120, low: 110, close: 118, volume: 800 },
    ]
    const positions: RNEA.ReadonlyNonEmptyArray<Position> = [Position.Sell, Position.Sell, Position.Hold]
    const initialAmount = 1000

    const result = backtest(bars, positions, initialAmount)

    expect(E.isLeft(result)).toBe(true)
    expect(result).toMatchObject(E.left(new Error('Cannot sell when currentTrade is not defined')))
  })
})
