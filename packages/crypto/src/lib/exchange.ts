import { Exchange, Ticker } from 'ccxt'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'

import { Market } from './market.ts'
import { Pair } from './pair.ts'

export const loadMarketsE = (exchange: Exchange): TE.TaskEither<Error, Record<Pair, Market>> => {
  return pipe(
    TE.tryCatch(async () => {
      return exchange.loadMarkets()
    }, E.toError),
    TE.map((markets) => {
      return markets as Record<Pair, Market>
    }),
  )
}

export const fetchTickersE = (exchange: Exchange): TE.TaskEither<Error, Record<Pair, Ticker>> => {
  return pipe(
    TE.tryCatch(async () => {
      return exchange.fetchTickers()
    }, E.toError),
    TE.map((markets) => {
      return markets as Record<Pair, Ticker>
    }),
  )
}
