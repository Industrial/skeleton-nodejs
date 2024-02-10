import { Exchange, Market } from 'ccxt'
import * as E from 'fp-ts/Either'
import { describe, expect, it } from 'vitest'

import { Pair } from '../lib/pair.ts'
import { getPairs, sortedPairs, Volume, writePairs } from '../lib/pairList.ts'

describe.skip('pairList', () => {
  // describe('filterWantedPairs', () => {
  //   describe('When called with an empty array', () => {
  //     it('should return an empty array', () => {
  //       const actual = filterWantedPairs([])
  //       expect(actual).toDeepEqual([])
  //     })
  //   })
  //   describe('When called with an array of pairs that contain unwanted entries', () => {
  //     it('should return a filtered array', () => {
  //       const actual = filterWantedPairs([
  //         createPair('BTC' as Base, 'USDT' as Quote),
  //         createPair('ETH3L' as Base, 'USDT' as Quote),
  //         createPair('ETH3S' as Base, 'USDT' as Quote),
  //         createPair('ETHUP' as Base, 'USDT' as Quote),
  //         createPair('ETHDOWN' as Base, 'USDT' as Quote),
  //         createPair('ETHSIDEWAYS' as Base, 'USDT' as Quote),
  //       ])
  //       expect(actual).toDeepEqual([
  //         createPair('BTC' as Base, 'USDT' as Quote),
  //         createPair('ETHSIDEWAYS' as Base, 'USDT' as Quote),
  //       ])
  //     })
  //   })
  // })

  describe('getPairs', () => {
    describe('When called with an exchange', () => {
      describe('When called with an exchange that returns a list of markets', () => {
        it('should return an array of pairs', async () => {
          const exchange = new Exchange()

          const mockLoadMarkets = stub(exchange, 'loadMarkets', returnsNext([
            Promise.resolve({
              BTC: { base: 'BTC', quote: 'USDT', active: true, type: 'spot' } as Market,
              ETH: { base: 'ETH', quote: 'USDT', active: true, type: 'spot' } as Market,
            }),
          ]))

          const pairs = await getPairs(exchange)()

          if (E.isLeft(pairs)) {
            throw pairs.left
          }

          expect(pairs.right).toDeepEqual([
            'BTC/USDT' as Pair,
            'ETH/USDT' as Pair,
          ])

          assertSpyCalls(mockLoadMarkets, 1)

          mockLoadMarkets.restore()
        })
      })
    })
  })

  describe('sortedPairs', () => {
    describe('When called with an array of pairs and volumes', () => {
      it('should return an array of pairs sorted by volume', () => {
        const pairs = [
          'BTC' as Pair,
          'ETH' as Pair,
          'SCHMERP' as Pair,
          'DERP' as Pair,
        ]

        const volumes = [
          100 as Volume,
          300 as Volume,
          200 as Volume,
          400 as Volume,
        ]

        const actual = sortedPairs(pairs, volumes)

        expect(actual).toDeepEqual([
          'DERP' as Pair,
          'ETH' as Pair,
          'SCHMERP' as Pair,
          'BTC' as Pair,
        ])
      })
    })
  })

  describe('writePairs', () => {
    describe('When called with a file path and an array of pairs', () => {
      it('should write the pairs to the file path', async () => {
        const pairs = [
          'BTC/USDT' as Pair,
          'ETH/USDT' as Pair,
        ]

        const mockWriteTextFile = stub(Deno, 'writeTextFile', returnsNext([
          Promise.resolve(),
        ]))

        await writePairs('test.json', pairs)

        assertSpyCalls(mockWriteTextFile, 1)
      })
    })
  })
})
