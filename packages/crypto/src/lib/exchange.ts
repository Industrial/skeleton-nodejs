import type { Exchange, Ticker } from 'ccxt'
import { Effect as Fx, pipe } from 'effect'

import type { Market } from './market.ts'
import type { Pair } from './pair.ts'

export const loadMarketsE = (
  exchange: Exchange,
): Fx.Effect<Record<Pair, Market>, Error> =>
  pipe(
    Fx.tryPromise(async () => exchange.loadMarkets()),
    Fx.map((markets) => markets as Record<Pair, Market>),
  )

export const fetchTickersE = (
  exchange: Exchange,
): Fx.Effect<Record<Pair, Ticker>, Error> =>
  pipe(
    Fx.tryPromise(async () => exchange.fetchTickers()),
    Fx.map((markets) => markets as Record<Pair, Ticker>),
  )
