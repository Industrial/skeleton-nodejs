import { describe, expect, it } from 'bun:test'
import {
  filterActiveMarkets,
  filterSpotMarkets,
  mapToPairs,
} from '../lib/market.ts'
import { type Base, type Pair, type Quote, createPair } from '../lib/pair.ts'
import { createMarket } from './test/market.ts'

describe('Market', () => {
  describe('filterActiveMarkets', () => {
    describe('When called with an empty array', () => {
      it('should return an empty array', async () => {
        const actual = filterActiveMarkets([])
        expect(actual).toStrictEqual([])
      })
    })

    describe('When called with an entries array of Pair and Market', () => {
      it('should return a filtered array', async () => {
        const actual = filterActiveMarkets([
          ['BTC/USDT' as Pair, createMarket('BTC', 'USDT', true, 'spot')],
          ['ETH/USDT' as Pair, createMarket('ETH', 'USDT', false, 'spot')],
        ])
        expect(actual).toStrictEqual([
          ['BTC/USDT' as Pair, createMarket('BTC', 'USDT', true, 'spot')],
        ])
      })
    })
  })

  describe('filterSpotMarkets', () => {
    describe('When called with an empty array', () => {
      it('should return an empty array', async () => {
        const actual = filterSpotMarkets([])
        expect(actual).toStrictEqual([])
      })
    })

    describe('When called with an entries array of Pair and Market', () => {
      it('should return a filtered array', async () => {
        const actual = filterSpotMarkets([
          ['BTC/USDT' as Pair, createMarket('BTC', 'USDT', true, 'spot')],
          ['ETH/USDT' as Pair, createMarket('ETH', 'USDT', false, 'margin')],
        ])
        expect(actual).toStrictEqual([
          ['BTC/USDT' as Pair, createMarket('BTC', 'USDT', true, 'spot')],
        ])
      })
    })
  })

  describe('mapToPairs', () => {
    describe('When called with an empty array', () => {
      it('should return an empty array', async () => {
        const actual = mapToPairs([])
        expect(actual).toStrictEqual([])
      })
    })

    describe('When called with an entries array of Pair and Market', () => {
      it('should return a filtered array', async () => {
        const actual = mapToPairs([
          ['BTC/USDT' as Pair, createMarket('BTC', 'USDT', true, 'spot')],
          ['ETH/USDT' as Pair, createMarket('ETH', 'USDT', true, 'spot')],
        ])
        expect(actual).toStrictEqual([
          createPair('BTC' as Base, 'USDT' as Quote),
          createPair('ETH' as Base, 'USDT' as Quote),
        ])
      })
    })
  })
})
