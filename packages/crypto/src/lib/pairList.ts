import { sequenceArrayWritable } from '@code9/either'
import { entries } from '@code9/record'
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
  (refinement: (x: Base) => boolean) => {
    return (pairs: Array<Pair>): Array<Pair> => {
      return pipe(
        pairs,
        A.filter((pair) => {
          return pipe(
            getBase(pair),
            E.map(refinement),
            E.match(() => {
              return true
            }, Boolean),
          )
        }),
      )
    }
  }

export const filterQuote =
  (refinement: (x: Quote) => boolean) => {
    return (pairs: Array<Pair>): Array<Pair> => {
      return pipe(
        pairs,
        A.filter((pair) => {
          return pipe(
            getQuote(pair),
            E.map(refinement),
            E.match(() => {
              return true
            }, Boolean),
          )
        }),
      )
    }
  }

export const filterByUnwantedBase = (base: Base): boolean => {
  return ['3L', '3S', 'UP', 'DOWN'].some((x) => {
    return base.endsWith(x)
  })
}

export const getPairs = (exchange: Exchange): TE.TaskEither<Error, Array<Pair>> => {
  return pipe(
    loadMarketsE(exchange),
    TE.chain((markets) => {
      return pipe(
        entries(markets),
        filterActiveMarkets,
        filterSpotMarkets,
        mapToPairs,
        sequenceArrayWritable,
        E.map(filterBase(filterByUnwantedBase)),
        TE.fromEither,
      )
    }),
  )
}

export const ordByVolume = pipe(
  N.Ord,
  Ord.reverse,
  contramap(([_, volume]: [Pair, Volume]) => {
    return volume
  }),
)

export const sortedPairs = (pairs: Array<Pair>, volumes: Array<Volume>): Array<Pair> => {
  return pipe(
    A.zip(pairs, volumes),
    A.sort(ordByVolume),
    A.map(([pair]) => {
      return pair
    }),
  )
}

export const writePairs = async (filePath: string, pairs: Array<Pair>): Promise<void> => {
  await fs.writeFile(filePath, JSON.stringify({ pairs }, null, 2))
}
