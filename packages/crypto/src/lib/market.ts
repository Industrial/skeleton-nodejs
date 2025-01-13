import type { Market as CCXTMarket } from 'ccxt'
import { Array as A, type Either as E, Effect as Fx, pipe } from 'effect'
import { type Base, type Pair, type Quote, createPair } from '../lib/pair.ts'

export type Market = NonNullable<CCXTMarket>

export const filterActiveMarkets = (
  markets: Array<[Pair, Market]>,
): Array<[Pair, Market]> =>
  pipe(
    markets,
    A.filter(([, value]) => Boolean(value.active)),
  )

export const filterSpotMarkets = (
  markets: Array<[Pair, Market]>,
): Array<[Pair, Market]> =>
  pipe(
    markets,
    A.filter(([, value]) => value.type === 'spot'),
  )

export const mapToPairs = (
  markets: Array<[Pair, Market]>,
): Array<E.Either<Pair, Error>> =>
  pipe(
    markets,
    A.map(([, value]) => createPair(value.base as Base, value.quote as Quote)),
  )
