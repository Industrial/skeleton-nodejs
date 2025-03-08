import { Args, Command } from '@effect/cli'
import { BunContext, BunRuntime } from '@effect/platform-bun'
import ccxt from 'ccxt'
import { Effect, Layer, pipe } from 'effect'
import type { ExchangeId } from './domain/ExchangeId'
import type { ExchangeSymbol } from './domain/ExchangeSymbol'
import { type Timeframe, TimeframeSchemaValues } from './domain/Timeframe'
import { CryptoDataService } from './service/CryptoDataService'

const exchangesEntries = Object.entries(ccxt.exchanges).map(([, y]) => [
  y,
  y,
]) as Array<[string, string]>

const timeframeEntries = TimeframeSchemaValues.map((x) => [x, x]) as Array<
  [string, string]
>

const ohlcvCommand = Command.make(
  'ohlcv',
  {
    exchange: Args.choice(exchangesEntries, {
      name: 'exchangeId',
    }),
    symbol: Args.text({ name: 'exchangeSymbol' }),
    timeframe: Args.choice(timeframeEntries, { name: 'timeframe' }),
    start: Args.date({ name: 'start' }),
    end: Args.date({ name: 'end' }),
  },
  (args) => {
    const exchangeId: ExchangeId = args.exchange as ExchangeId
    const symbol: ExchangeSymbol = args.symbol
    const timeframe: Timeframe = args.timeframe as Timeframe
    const start = args.start
    const end = args.end

    return Effect.gen(function* () {
      const crypto = yield* CryptoDataService
      const result = yield* crypto.getOHLCV(
        exchangeId,
        symbol,
        timeframe,
        start,
        end,
      )
      console.log('result', result)
    })
  },
)

const cli = Command.run(ohlcvCommand, {
  name: 'Trader',
  version: 'v1.0.0',
})

pipe(
  Effect.suspend(() => cli(process.argv)),
  // @ts-ignore
  Effect.provide(Layer.merge(BunContext.layer, CryptoDataService.Live)),
  BunRuntime.runMain,
)
