import { describe, expect, it, spyOn } from 'bun:test'
import { Exchange } from 'ccxt'
import { Either as E, Effect as Fx } from 'effect'
import { loadMarketsE } from '../lib/exchange.ts'
import { createMarket } from './test/market.ts'

describe('Exchange', () => {
  const exchange = new Exchange()

  describe('loadMarketsE', () => {
    describe('When called with an exchange and it produces an error', () => {
      it('should return the error', async () => {
        const mockError = new Error('Failed to load markets')
        const mockLoadMarkets = spyOn(
          exchange,
          'loadMarkets',
        ).mockImplementationOnce(async () => {
          return await Promise.reject(mockError)
        })
        // TODO: Fix this generic message. It should be the specific one above.
        expect(async () => {
          await Fx.runPromise(loadMarketsE(exchange))
        }).toThrowError(
          new Error('An unknown error occurred in Effect.tryPromise'),
        )
        mockLoadMarkets.mockRestore()
      })
    })

    describe('When called with an exchange', () => {
      it('should return the markets', async () => {
        const expectedMarkets = {
          BTC: createMarket('BTC', 'USDT', true, 'spot'),
          ETH: createMarket('ETH', 'USDT', true, 'spot'),
        }
        const mockLoadMarkets = spyOn(
          exchange,
          'loadMarkets',
        ).mockImplementationOnce(
          async () => await Promise.resolve(expectedMarkets),
        )
        const actual = await Fx.runPromise(loadMarketsE(exchange))
        expect(actual).toStrictEqual(expectedMarkets)
        expect(mockLoadMarkets).toHaveBeenCalledTimes(1)
        mockLoadMarkets.mockRestore()
      })
    })
  })
})
