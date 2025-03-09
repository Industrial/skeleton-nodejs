import { Args, Command } from '@effect/cli'
import { BunContext, BunRuntime } from '@effect/platform-bun'
import ccxt from 'ccxt'
import { Effect, Layer, pipe } from 'effect'
import type { BacktestParameters } from './domain/backtesting/BacktestParameters'
import { PositionSizingMethod } from './domain/backtesting/BacktestParameters'
import type { ExchangeId } from './domain/market-data/ExchangeId'
import type { Pair } from './domain/market-data/Pair'
import {
  type Timeframe,
  TimeframeSchemaValues,
} from './domain/market-data/Timeframe'
import { PriceType } from './domain/strategy/Indicator'
import type { Strategy } from './domain/strategy/Strategy'
import {
  MovingAverageType,
  createMovingAverageCrossoverStrategy,
} from './domain/strategy/strategies/MovingAverageCrossover'
import { BacktestingService } from './service/BacktestingService'
import { CryptoDataService } from './service/CryptoDataService'

const exchangesEntries = Object.entries(ccxt.exchanges).map(([, y]) => [
  y,
  y,
]) as Array<[string, string]>

const timeframeEntries = TimeframeSchemaValues.map((x) => [x, x]) as Array<
  [string, string]
>

const positionSizingMethodEntries = Object.entries(PositionSizingMethod).map(
  ([, value]) => [value, value],
) as Array<[string, string]>

const strategyEntries = [
  ['MovingAverageCrossover', 'MovingAverageCrossover'],
] as Array<[string, string]>

const maTypeEntries = Object.entries(MovingAverageType).map(([, value]) => [
  value,
  value,
]) as Array<[string, string]>

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
    const symbol: Pair = args.symbol
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

const backtestCommand = Command.make(
  'backtest',
  {
    exchange: Args.choice(exchangesEntries, {
      name: 'exchangeId',
    }),
    symbol: Args.text({ name: 'exchangeSymbol' }),
    timeframe: Args.choice(timeframeEntries, { name: 'timeframe' }),
    start: Args.date({ name: 'start' }),
    end: Args.date({ name: 'end' }),
    strategy: Args.choice(strategyEntries, { name: 'strategy' }),
    initialCapital: Args.text({ name: 'initialCapital' }),
    feeRate: Args.text({ name: 'feeRate' }),
    slippageRate: Args.text({ name: 'slippageRate' }),
    positionSizingMethod: Args.choice(positionSizingMethodEntries, {
      name: 'positionSizingMethod',
    }),
    positionSizeValue: Args.text({ name: 'positionSizeValue' }),
    reinvestProfits: Args.boolean({
      name: 'reinvestProfits',
    }),
    maxConcurrentPositions: Args.text({
      name: 'maxConcurrentPositions',
    }),
    fastPeriod: Args.text({ name: 'fastPeriod' }),
    slowPeriod: Args.text({ name: 'slowPeriod' }),
    maType: Args.choice(maTypeEntries, { name: 'maType' }),
    name: Args.text({ name: 'name' }),
    description: Args.text({
      name: 'description',
    }),
  },
  (args) => {
    const exchangeId: ExchangeId = args.exchange as ExchangeId
    const symbol: Pair = args.symbol
    const timeframe: Timeframe = args.timeframe as Timeframe
    const start = args.start
    const end = args.end
    const strategyName = args.strategy
    const initialCapital = Number.parseFloat(args.initialCapital || '10000')
    const feeRate = Number.parseFloat(args.feeRate || '0.001')
    const slippageRate = Number.parseFloat(args.slippageRate || '0.001')
    const positionSizingMethod =
      (args.positionSizingMethod as PositionSizingMethod) ||
      PositionSizingMethod.PercentageOfCapital
    const positionSizeValue = Number.parseFloat(args.positionSizeValue || '10')
    const reinvestProfits = args.reinvestProfits ?? true
    const maxConcurrentPositions = Number.parseInt(
      args.maxConcurrentPositions || '0',
      10,
    )
    const fastPeriod = Number.parseInt(args.fastPeriod || '12', 10)
    const slowPeriod = Number.parseInt(args.slowPeriod || '26', 10)
    const maType =
      (args.maType as MovingAverageType) || MovingAverageType.Simple
    const name = args.name || 'Backtest'
    const description = args.description || 'Backtest run'

    return Effect.gen(function* () {
      // Get market data
      const crypto = yield* CryptoDataService
      const candlesticks = yield* crypto.getOHLCV(
        exchangeId,
        symbol,
        timeframe,
        start,
        end,
      )

      console.log(
        `Retrieved ${candlesticks.length} candlesticks for backtesting`,
      )

      // Create strategy
      let strategy: Strategy
      if (strategyName === 'MovingAverageCrossover') {
        strategy = yield* createMovingAverageCrossoverStrategy({
          name: 'MA Crossover',
          description: 'Moving Average Crossover Strategy',
          fastPeriod,
          slowPeriod,
          priceType: PriceType.Close,
          maType,
        })
      } else {
        throw new Error(`Unknown strategy: ${strategyName}`)
      }

      // Create backtest parameters
      const parameters: BacktestParameters = {
        initialCapital,
        feeRate,
        slippageRate,
        positionSizingMethod,
        positionSizeValue,
        reinvestProfits,
        maxConcurrentPositions,
      }

      // Run backtest
      const backtesting = yield* BacktestingService
      const result = yield* backtesting.runBacktest(
        strategy,
        candlesticks,
        parameters,
        name,
        description,
      )

      // Display results
      console.log(`Backtest completed: ${result.name}`)
      console.log(
        `Total return: ${result.metrics.totalReturn.toFixed(
          2,
        )} (${result.metrics.totalReturnPercentage.toFixed(2)}%)`,
      )
      console.log(`Win rate: ${result.metrics.winRate.toFixed(2)}%`)
      console.log(`Profit factor: ${result.metrics.profitFactor.toFixed(2)}`)
      console.log(
        `Max drawdown: ${result.metrics.maxDrawdownPercentage.toFixed(2)}%`,
      )
      console.log(`Total trades: ${result.metrics.numberOfTrades}`)

      // Save result
      const resultId = yield* backtesting.saveBacktestResult(result)
      console.log(`Backtest result saved with ID: ${resultId}`)

      return result
    })
  },
)

// Create a main command with subcommands
const mainCommand = Command.make('main', {}, () =>
  Effect.succeed('Use a subcommand: ohlcv or backtest'),
).pipe(Command.withSubcommands([ohlcvCommand, backtestCommand]))

const cli = Command.run(mainCommand, {
  name: 'Trader',
  version: 'v1.0.0',
})

pipe(
  Effect.suspend(() => cli(process.argv)),
  // @ts-ignore
  Effect.provide(
    Layer.merge(
      Layer.merge(BunContext.layer, CryptoDataService.Live),
      BacktestingService.Live,
    ),
  ),
  BunRuntime.runMain,
)
