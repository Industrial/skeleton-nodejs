#!/usr/bin/env -S node --import @swc-node/register/esm-register
import 'dotenv/config'

import { kucoin, Ticker } from 'ccxt'
import * as A from 'fp-ts/Array'
import { isLeft } from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as N from 'fp-ts/number'
import * as Ord from 'fp-ts/Ord'

import { firstElement } from '../lib/array.ts'
import { fetchTickersE } from '../lib/exchange.ts'
import { Pair } from '../lib/pair.ts'
import { getPairs, Volume } from '../lib/pairList.ts'
import { entries } from '../lib/record.ts'

const Kucoin = kucoin
const exchange = new Kucoin()

// const x = pipe(
//   getPairs(exchange),
//   TE.map((pairs) =>
//     pipe(
//       pairs,
//       filterQuote((quote) => quote === 'USDT'),
//     )
//   )
// );

// const y = pipe(
//   fetchTickersE(exchange),
//   TE.map((tickers) =>
//     pipe(
//       tickers,
//       Object.entries,
//       filterQuote((quote) => quote === 'USDT'),
//     )
//   )
// )

const pairs = await getPairs(exchange)()
if (isLeft(pairs)) {
  throw pairs.left
}

const tickers = await fetchTickersE(exchange)()
if (isLeft(tickers)) {
  throw tickers.left
}

const byIndexOf = <A, B>(f: (x: B) => A) => (as: Array<A>) =>
  pipe(N.Ord,
    Ord.contramap((b: B) => as.indexOf(f(b))))

// const byPairIndex = pipe(
//   N.Ord,
//   Ord.contramap(([pair, ticker]: [Pair, Ticker]) => pairs.right.indexOf(pair))
// );

const x = firstElement<[Pair, Ticker]>

const y = byIndexOf(x)

const volumes = pipe(tickers.right,
  entries,
  A.filter(([key, value]) => pairs.right.some((pair) => pair === key)),
  A.sortBy([y]),
  A.map(([key, value]) => value.quoteVolume as Volume))

// const sortedPairsByVolume = (entries: Array<[Pair, Ticker]>) =>
//   pipe(
//     entries,
//     A.map(([pair, ticker]) => [pair, Number(ticker.quoteVolume) as Volume]),
//   );

// const volumes = Object.entries(tickers)
//   .filter(([key, value]) => filteredPairs.some(pair => pair === key))
//   .sort((a, b) => filteredPairs.indexOf(a[0] as Pair) - filteredPairs.indexOf(b[0] as Pair))
//   .map(([key, value]) => value.quoteVolume as Volume);
// const sorted = sortedPairs(filteredPairs, volumes);

// await writePairs(`./user_data/strategies/${STRATEGY}/pair_list.json`, sorted);
