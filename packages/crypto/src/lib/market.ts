import type { Market as CCXTMarket } from 'ccxt'
import * as A from 'fp-ts/Array'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'

import { createPair, Pair } from '../lib/pair.ts'

export type Market = NonNullable<CCXTMarket>

export const filterActiveMarkets = (markets: Array<[Pair, Market]>): Array<[Pair, Market]> => {
  return pipe(
    markets,
    A.filter(([, value]) => {
      return Boolean(value.active)
    }),
  )
}

export const filterSpotMarkets = (markets: Array<[Pair, Market]>): Array<[Pair, Market]> => {
  return pipe(
    markets,
    A.filter(([, value]) => {
      return value.type === 'spot'
    }),
  )
}

export const mapToPairs = (markets: Array<[Pair, Market]>): Array<E.Either<Error, Pair>> => {
  return pipe(
    markets,
    A.map(([, value]) => {
      return createPair(value.base, value.quote)
    }),
  )
}
