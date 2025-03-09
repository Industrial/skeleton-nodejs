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
      const candlesticks = yield* crypto.getOHLCV(
        exchangeId,
        symbol,
        timeframe,
        start,
        end,
      )

      // We can now work directly with domain models
      console.log(`Retrieved ${candlesticks.length} candlesticks`)

      // Example of using domain properties
      if (candlesticks.length > 0) {
        const first = candlesticks[0]
        const last = candlesticks[candlesticks.length - 1]
        console.log(
          `First candlestick: ${new Date(first.timestamp).toISOString()}`,
        )
        console.log(
          `Last candlestick: ${new Date(last.timestamp).toISOString()}`,
        )
        console.log(
          `Price range: ${Math.min(
            ...candlesticks.map((c) => c.low),
          )} - ${Math.max(...candlesticks.map((c) => c.high))}`,
        )
      }
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
