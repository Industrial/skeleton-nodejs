import { sequenceArrayWritable } from '@code9/either'
import { Exchange } from 'ccxt'
import * as A from 'fp-ts/Array'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as N from 'fp-ts/number'
import * as Ord from 'fp-ts/Ord'
import { contramap } from 'fp-ts/Ord'
import * as TE from 'fp-ts/TaskEither'
import fs from 'fs/promises'
import { Opaque } from 'type-fest'

import { loadMarketsE } from './exchange.ts'
import { filterActiveMarkets, filterSpotMarkets, mapToPairs } from './market.ts'
import { Base, getBase, getQuote, Pair, Quote } from './pair.ts'

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
        A.filter((pair) =>
          pipe(
            getBase(pair),
            E.map(refinement),
            E.match(() =>
              true, Boolean),
          )),
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
            E.match(() =>
              true, Boolean),
          )),
      )

export const filterByUnwantedBase = (base: Base): boolean =>
  ['3L', '3S', 'UP', 'DOWN'].some((x) =>
    base.endsWith(x))

export const getPairs = (exchange: Exchange): TE.TaskEither<Error, Array<Pair>> =>
  pipe(
    loadMarketsE(exchange),
    TE.chain((markets) =>
      pipe(
        Object.entries(markets),
        filterActiveMarkets,
        filterSpotMarkets,
        mapToPairs,
        sequenceArrayWritable,
        E.map(filterBase(filterByUnwantedBase)),
        TE.fromEither,
      )),
  )

export const ordByVolume = pipe(
  N.Ord,
  Ord.reverse,
  contramap(([_, volume]: [Pair, Volume]) =>
    volume),
)

export const sortedPairs = (pairs: Array<Pair>, volumes: Array<Volume>): Array<Pair> =>
  pipe(
    A.zip(pairs, volumes),
    A.sort(ordByVolume),
    A.map(([pair]) =>
      pair),
  )

export const writePairs = async (filePath: string, pairs: Array<Pair>): Promise<void> => {
  await fs.writeFile(filePath, JSON.stringify({ pairs }, null, 2))
}
