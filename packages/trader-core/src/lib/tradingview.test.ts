import { describe, expect, it } from 'bun:test'
import * as E from 'fp-ts/Either'
import * as FN from 'fp-ts/function'

import { createGetBatchBarsTE, fetchBarsInBatches, getSymbolInfo, getTradingView, TV, TVSymbolInfo } from './tradingview.ts'

const createGetBars = (): TV['datafeed']['getBars'] =>
  async (symbolInfo, resolution, range, resolve, _reject) => {
    resolve([
      {
        time: 100,
        open: 100,
        high: 100,
        low: 100,
        close: 100,
        volume: 100,
      },
    ])
  }

const createResolveSymbol = (): TV['datafeed']['resolveSymbol'] =>
  async (_symbolName, resolve, _reject) => {
    resolve({
    // eslint-disable-next-line camelcase
      has_daily: false,
      // eslint-disable-next-line camelcase
      has_seconds: false,
      // eslint-disable-next-line camelcase
      logo_urls: [],
      // eslint-disable-next-line camelcase
      visible_plots_set: 'ohlcv',
      description: 'Apple Inc.',
      exchange: 'NASDAQ',
      // eslint-disable-next-line camelcase
      has_intraday: true,
      minmov: 1,
      name: 'AAPL',
      pricescale: 100,
      session: '0930-1600',
      // eslint-disable-next-line camelcase
      supported_resolutions: ['1', '3', '5', '15', '30', '60', '120', '240', '720', '1D', '1W', '1M'],
      timezone: 'America/New_York',
    })
  }

const createTV = (): TV => {
  return {
    datafeed: {
      getBars: createGetBars(),
      resolveSymbol: createResolveSymbol(),
    },
  }
}

describe('tradingview', () => {
  describe('getTradingView', () => {
    describe('When the TradingView object does not exist on the global scope', () => {
      it('should throw an error', () => {
        const expected = 'TradingView Not Found'
        const actual = () =>
          getTradingView()
        expect(actual).toThrowError(expected)
      })
    })

    describe('When the TradingView object exists on the global scope', () => {
      it('should return the TradingView object', () => {
        const expected = 'tradingview_123'
        // eslint-disable-next-line camelcase
        globalThis.tradingview_123 = 'tradingview_123'
        const actual = getTradingView()
        expect(actual).toEqual(expected)
      })
    })
  })

  describe('getSymbolInfo', () => {
    describe('When passed the TradingView object and a symbol name', () => {
      it('should return the symbol info', async () => {
        const tv = createTV()
        const symbolName = 'AAPL'
        const expected: TVSymbolInfo = {
          // eslint-disable-next-line camelcase
          has_daily: false,
          // eslint-disable-next-line camelcase
          has_seconds: false,
          // eslint-disable-next-line camelcase
          logo_urls: [],
          // eslint-disable-next-line camelcase
          visible_plots_set: 'ohlcv',
          description: 'Apple Inc.',
          exchange: 'NASDAQ',
          // eslint-disable-next-line camelcase
          has_intraday: true,
          minmov: 1,
          name: 'AAPL',
          pricescale: 100,
          session: '0930-1600',
          // eslint-disable-next-line camelcase
          supported_resolutions: ['1', '3', '5', '15', '30', '60', '120', '240', '720', '1D', '1W', '1M'],
          timezone: 'America/New_York',
        }
        const actual = await getSymbolInfo(tv, symbolName)
        expect(actual).toEqual(expected)
      })
    })
  })

  describe('createGetBatchBarsTE', () => {
    describe('When passed the TradingView object, symbol info, timeframe, and limit', () => {
      it('should return a function that fetches one batch of bars', async () => {
        const tv = createTV()
        const symbolInfo = await new Promise<TVSymbolInfo>((resolve, reject) => {
          createResolveSymbol()('BTC/USDT', resolve, reject)
        })
        const timeframe = '1h'
        const limit = 100
        const expected = [
          {
            time: 100,
            open: 100,
            high: 100,
            low: 100,
            close: 100,
            volume: 100,
          },
        ]
        const getBatchBarsTE = createGetBatchBarsTE(tv, symbolInfo, timeframe, limit)
        const actualTE = await getBatchBarsTE(new Date(), new Date())()
        FN.pipe(
          actualTE,
          E.fold(
            (error) => {
              throw error
            },
            (actual) => {
              expect(actual).toEqual(expected)
            },
          ),
        )
      })
    })
  })

  describe('fetchBarsInBatches', () => {
    describe('When passed the TradingView object, symbol info, timeframe, count back, and limit', () => {
      it('should return an array of bars', async () => {
        const tv = createTV()
        const symbolInfo = await new Promise<TVSymbolInfo>((resolve, reject) => {
          createResolveSymbol()('BTC/USDT', resolve, reject)
        })
        const timeframe = '1h'
        const countBack = 100
        const limit = 100
        const expected = [
          {
            time: 100,
            open: 100,
            high: 100,
            low: 100,
            close: 100,
            volume: 100,
          },
        ]
        const actual = await fetchBarsInBatches(tv, symbolInfo, timeframe, countBack, limit)()
        if (E.isLeft(actual)) {
          throw actual.left
        }
        expect(actual.right).toEqual(expected)
      })
    })
  })

  // describe('get', () => {
  //   describe('when the key does not exist', () => {
  //     it('should return undefined', () => {
  //       const getSpy = vi.spyOn(window, 'localStorage', 'get')
  //       const expected = undefined
  //       const actual = get('test')
  //       expect(actual).toEqual(expected)
  //       expect(getSpy).toHaveBeenCalledTimes(1)
  //     })
  //   })
  // })

  // describe('set', () => {
  //   describe('when setting a key', () => {
  //     it('should return the value through get', () => {
  //       const expected = 'derp'
  //       set('herp', 'derp')
  //       const actual = get('herp')
  //       expect(actual).toEqual(expected)
  //     })
  //   })
  // })

  // describe('del', () => {
  //   describe('when deleting a key', () => {
  //     it('should delete the value in the localStorage', () => {
  //       const expected = undefined
  //       set('herp', 'derp')
  //       del('herp')
  //       const actual = get('herp')
  //       expect(actual).toEqual(expected)
  //     })
  //   })
  // })
})

