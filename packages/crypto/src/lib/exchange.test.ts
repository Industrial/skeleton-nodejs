

import { describe, expect, it, spyOn } from 'bun:test'
import { Exchange } from 'ccxt'
import * as E from 'fp-ts/Either'

import { loadMarketsE } from '../lib/exchange.ts'
import { createMarket } from './test/market.ts'

describe('Exchange', () => {
  const exchange = new Exchange()

  describe('loadMarketsE', () => {
    describe('When called with an exchange and it produces an error', () => {
      it('should return the error', async () => {
        const mockError = new Error('Failed to load markets')
        const mockLoadMarkets = spyOn(exchange, 'loadMarkets').mockImplementationOnce(async () =>
          Promise.reject(mockError))
        const actual = await loadMarketsE(exchange)()
        if (E.isRight(actual)) {
          throw new Error('Expected a failure, but got a success')
        }
        expect(E.isLeft(actual)).toStrictEqual(true)
        expect(actual.left).toStrictEqual(mockError)
        expect(mockLoadMarkets).toHaveBeenCalledTimes(1)
        mockLoadMarkets.mockRestore()
      })
    })

    describe('When called with an exchange', () => {
      it('should return the markets', async () => {
        const expectedMarkets = {
          BTC: createMarket('BTC', 'USDT', true, 'spot'),
          ETH: createMarket('ETH', 'USDT', true, 'spot'),
        }
        const mockLoadMarkets = spyOn(exchange, 'loadMarkets').mockImplementationOnce(async () =>
          Promise.resolve(expectedMarkets))
        const actual = await loadMarketsE(exchange)()
        if (E.isLeft(actual)) {
          throw actual.left
        }
        expect(E.isRight(actual)).toStrictEqual(true)
        expect(actual.right).toStrictEqual(expectedMarkets)
        expect(mockLoadMarkets).toHaveBeenCalledTimes(1)
        mockLoadMarkets.mockRestore()
      })
    })
  })
})
