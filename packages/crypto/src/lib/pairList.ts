import fs from 'node:fs/promises'
import type { Exchange, MarketInterface } from 'ccxt'
import { Array as A, Either as E, Effect as Fx, Order, pipe } from 'effect'
import type { Opaque } from 'type-fest'

import { loadMarketsE } from './exchange.ts'
import { filterActiveMarkets, filterSpotMarkets, mapToPairs } from './market.ts'
import type { Base, Pair, Quote } from './pair.ts'
import { getBase, getQuote } from './pair.ts'

export type Date = Opaque<number, 'Date'>
export type Open = Opaque<number, 'Open'>
export type High = Opaque<number, 'High'>
export type Low = Opaque<number, 'Low'>
export type Close = Opaque<number, 'Close'>
export type Volume = Opaque<number, 'Volume'>

export const filterBase =
  (refinement: (x: Base) => boolean) =>
  (pairs: Array<Pair>): Array<Pair> =>
    pipe(
      pairs,
      A.filter((a) =>
        pipe(
          getBase(a),
          E.map(refinement),
          E.match({
            onLeft: () => false,
            onRight: (x) => x,
          }),
        ),
      ),
    )

export const filterQuote =
  (refinement: (x: Quote) => boolean) =>
  (pairs: Array<Pair>): Array<Pair> =>
    pipe(
      pairs,
      A.filter((pair) =>
        pipe(
          getQuote(pair),
          E.map(refinement),
          E.match({
            onLeft: () => false,
            onRight: (x) => x,
          }),
        ),
      ),
    )

export const filterByUnwantedBase = (base: Base): boolean =>
  ['3L', '3S', 'UP', 'DOWN'].some((x) => base.endsWith(x))

export const flattenE = <A>(
  as: Array<E.Either<A, Error>>,
): E.Either<Array<A>, Error> =>
  pipe(
    as,
    A.reduce(E.right([] as Array<A>) as E.Either<Array<A>, Error>, (acc, a) => {
      const x = E.zipWith(acc, a, (as, a) => [...as, a])
      return x
    }),
  )

export const getPairs = (
  exchange: Exchange,
): Fx.Effect<Array<Pair>, Error, unknown> =>
  pipe(
    loadMarketsE(exchange),
    Fx.flatMap((markets) =>
      pipe(
        Object.entries(markets) as Array<[Pair, MarketInterface]>,
        filterActiveMarkets,
        filterSpotMarkets,
        mapToPairs,
        flattenE,
      ),
    ),
  )

export const ordByVolume = Order.mapInput(
  Order.number,
  ([, volume]: [Pair, Volume]) => volume,
)

export const sortedPairs = (
  pairs: Array<Pair>,
  volumes: Array<Volume>,
): Array<Pair> =>
  pipe(
    A.zip(pairs, volumes),
    A.sort(ordByVolume),
    A.map(([pair]) => pair),
  )

export const writePairs = async (
  filePath: string,
  pairs: Array<Pair>,
): Promise<void> => {
  await fs.writeFile(filePath, JSON.stringify({ pairs }, null, 2))
}
