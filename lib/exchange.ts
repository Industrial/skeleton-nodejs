import { Exchange, Ticker } from 'ccxt'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'

import { Market } from '@/lib/market.ts'
import { Pair } from '@/lib/pair.ts'

export const loadMarketsE = (exchange: Exchange): TE.TaskEither<Error, Record<Pair, Market>> =>
  pipe(TE.tryCatch(async () => exchange.loadMarkets(), E.toError),
    TE.map((markets) => markets as Record<Pair, Market>))

export const fetchTickersE = (exchange: Exchange): TE.TaskEither<Error, Record<Pair, Ticker>> =>
  pipe(TE.tryCatch(async () => exchange.fetchTickers(), E.toError),
    TE.map((markets) => markets as Record<Pair, Ticker>))
