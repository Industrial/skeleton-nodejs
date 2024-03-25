import { getRandomNumber } from '@code9/number'
import { strategySimulatedAnnealing } from '@code9/simulated-annealing'
import { backtest, logProfitPercentages, logTrades } from '@code9/trader-backtest'
import * as localstorage from '@code9/trader-core'
import { fetchBarsInBatches, getSymbol, getSymbolInfo, getTradingView, log, Maybe, millisecondsUntilNextTimeframe, normalizePositions, start, StrategyOptionsBounds, Timeframe, toMs, Trade } from '@code9/trader-core'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as RNEA from 'fp-ts/ReadonlyNonEmptyArray'

import { rsiSmaStrategy } from './rsiStrategy.ts'

export type RSISMAAnnealingState = {
  rsiLength: number
  rsiLowerLimit: number
  rsiUpperLimit: number
  smaLength: number
}

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    // Timeframe used for backtest
    BT_TIMEFRAME: Timeframe

    // Amount of bars to fetch.
    BT_COUNTBACK: number

    // Transaction cost percentage.
    BT_TRANSACTION_COST_PERCENTAGE: number

    // Initial quote currency (We start with 0 for base).
    BT_INITIAL_QUOTE: number

    // Webhook for Discord messages
    BT_DISCORD_WEBHOOK_URL: string
  }
}

// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
window.BT_TIMEFRAME ||= '15m'

// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
window.BT_COUNTBACK ||= 3000

// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
window.BT_TRANSACTION_COST_PERCENTAGE ||= 0.25

// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
window.BT_INITIAL_QUOTE ||= 1

// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
window.BT_DISCORD_WEBHOOK_URL ||= `https://discord.com/api/webhooks/1141642017584185356/Y_mHZDhzzG1JGS6SA7vZ7UdZagTrgLhAYB7dXfOcw2xTt_PlBoPqcom_k0IQvtYhNzm-`

const main = async (): Promise<void> => {
  const symbol = getSymbol()
  const optionsKey = `${symbol}_${window.BT_TIMEFRAME}_options`
  const tradesKey = `${symbol}_${window.BT_TIMEFRAME}_trades`
  const discordBotWebhookURL = window.BT_DISCORD_WEBHOOK_URL

  const tv = getTradingView()
  const symbolInfo = await getSymbolInfo(tv, symbol)
  log.info('symbolInfo', symbolInfo)

  let options: Maybe<RSISMAAnnealingState>
  let trades: Array<Trade> = []

  // Load the options and trades from LocalStorage as a starting point for the annealing
  const storedOptions = localstorage.get(optionsKey)
  if (storedOptions) {
    options = JSON.parse(String(storedOptions)) as RSISMAAnnealingState
  }
  const storedTrades = localstorage.get(tradesKey)
  if (storedTrades) {
    trades = JSON.parse(String(storedTrades)) as Array<Trade>
  }

  const work = async () => {
    try {
      const bars = await fetchBarsInBatches(tv, symbolInfo, window.BT_TIMEFRAME, window.BT_COUNTBACK, 1000)
      const maximumAnnealingIterations = window.BT_COUNTBACK - bars.length + 1000

      log.info(`Transaction Cost Percentage: ${window.BT_TRANSACTION_COST_PERCENTAGE}%`)
      log.info(`Bars: ${bars.length}`)
      log.info(`Timeframe: ${window.BT_TIMEFRAME}`)
      log.info(`Initial Quote: ${window.BT_INITIAL_QUOTE}`)
      log.info(`Maximum Annealing Iterations: ${maximumAnnealingIterations}`)

      // let newOptions: Maybe<RSISMAAnnealingState>
      // let newTrades: Maybe<Array<Trade>>
      if (!options || trades.length === 0) {
        const bounds: StrategyOptionsBounds<RSISMAAnnealingState> = {
          rsiLength: [2, 20],
          rsiLowerLimit: [20, 40],
          rsiUpperLimit: [60, 80],
          smaLength: [1, 20],
        }

        const getInitialState = () => {
          return {
            rsiLength: getRandomNumber(Number(bounds.rsiLength[0]), Number(bounds.rsiLength[1])),
            rsiLowerLimit: getRandomNumber(Number(bounds.rsiLowerLimit[0]), Number(bounds.rsiLowerLimit[1])),
            rsiUpperLimit: getRandomNumber(Number(bounds.rsiUpperLimit[0]), Number(bounds.rsiUpperLimit[1])),
            smaLength: getRandomNumber(Number(bounds.smaLength[0]), Number(bounds.smaLength[1])),
          }
        }

        // eslint-disable-next-line require-atomic-updates
        options = await strategySimulatedAnnealing<RSISMAAnnealingState>(
          bars,
          maximumAnnealingIterations,
          window.BT_INITIAL_QUOTE,
          window.BT_TRANSACTION_COST_PERCENTAGE,
          bounds,
          getInitialState,
          rsiSmaStrategy,
        )
        const positions = RNEA.fromArray(normalizePositions(rsiSmaStrategy(options)(bars)))
        if (O.isNone(positions)) {
          throw new Error('No positions found')
        }

        const newTrades = backtest(
          bars,
          positions.value,
          window.BT_INITIAL_QUOTE,
          window.BT_TRANSACTION_COST_PERCENTAGE,
        )
        if (E.isLeft(newTrades)) {
          throw newTrades.left
        }
        // eslint-disable-next-line require-atomic-updates
        trades = newTrades.right
        log.info('Options: ', options)
      } else {
        // // Temporarily turn off anealing, just take the localstorage values.
        // const lastTrade: Maybe<Trade> = trades[trades.length - 1]
        // if (typeof lastTrade === 'undefined') {
        //   throw new Error(`No last trade`)
        // } else if (lastTrade.endDate > lastTrade.startDate) {
        //   newOptions = await performAnnealing(
        //     bars,
        //     maximumAnnealingIterations,
        //     window.BT_INITIAL_QUOTE,
        //     window.BT_TRANSACTION_COST_PERCENTAGE,
        //   )
        //   const positions = normalizePositions(rsiSmaStrategy(newOptions)(bars))
        //   newTrades = backtest(bars, positions, window.BT_INITIAL_QUOTE, window.BT_TRANSACTION_COST_PERCENTAGE)
        //   const newLastTrade: Maybe<Trade> = newTrades[newTrades.length - 1]
        //   if (typeof newLastTrade !== 'undefined') {
        //     if (newLastTrade.quote > lastTrade.quote) {
        //       // eslint-disable-next-line require-atomic-updates
        //       options = newOptions
        //       // eslint-disable-next-line require-atomic-updates
        //       trades = newTrades
        //     }
        //   }
        // }

        const positions = RNEA.fromArray(normalizePositions(rsiSmaStrategy(options)(bars)))
        if (O.isNone(positions)) {
          throw new Error('No positions found')
        }

        const newTrades = backtest(
          bars,
          positions.value,
          window.BT_INITIAL_QUOTE,
          window.BT_TRANSACTION_COST_PERCENTAGE,
        )
        if (E.isLeft(newTrades)) {
          throw newTrades.left
        }
        trades = newTrades.right
      }

      log.info('Options: ', options)
      localstorage.set(optionsKey, JSON.stringify(options))
      localstorage.set(tradesKey, JSON.stringify(trades))

      logTrades(trades, window.BT_INITIAL_QUOTE)
      logProfitPercentages(trades, window.BT_INITIAL_QUOTE)
      const lastTrade = trades[trades.length - 1]
      if (typeof lastTrade === 'undefined') {
        throw new Error(`No trades`)
      } else {
        const now = new Date(Date.now())
        const startOfCurrentTimeframe = start(window.BT_TIMEFRAME, new Date(Date.now()))
        log.info(startOfCurrentTimeframe.toISOString(), 'startOfCurrentTimeframe')
        const startDate = new Date(lastTrade.startDate)
        log.info(startDate.toISOString(), 'startDate')
        const endDate = new Date(lastTrade.endDate)
        log.info(endDate.toISOString(), 'endDate')

        if (startOfCurrentTimeframe === startDate) {
          log.info('Posting BUY to Discord...')
          await fetch(discordBotWebhookURL, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              content: `${now.toISOString()} BUY ${symbol} ${lastTrade.startPrice}`,
            }),
          })
        } else if (startOfCurrentTimeframe === endDate) {
          log.info('Posting SELL to Discord...')
          await fetch(discordBotWebhookURL, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
              content: `${now.toISOString()} SELL ${symbol} ${lastTrade.endPrice}`,
            }),
          })
        }
      }
    } catch (error: unknown) {
      console.error(error)
    }
  }

  const now = new Date(Date.now())
  const timeout = millisecondsUntilNextTimeframe(window.BT_TIMEFRAME, now)
  log.info(`Waiting for ${timeout / 1000 / 60} minutes...`)
  setTimeout(async () => {
    const interval = toMs(window.BT_TIMEFRAME)
    log.info(`Setting interval for ${interval / 1000 / 60} minutes...`)
    setInterval(async () => {
      await work()
    }, interval)
    await work()
  }, timeout)
  await work()
}

main().catch((error) => {
  log.error(error)
})

// // Create a new container element for the new widget.
// const divElement = document.createElement('div')
// divElement.id = '#mycontainer'
// document.body.appendChild(divElement)
// // Create TradingView widget
// // @ts-expect-error error
// const widget = new window.TradingView.widget({
//   width: 980,
//   height: 610,
//   symbol,
//   interval: window.BT_TIMEFRAME,
//   timezone: 'Etc/UTC',
//   theme: 'Dark',
//   style: '1',
//   locale: 'en',
//   toolbar_bg: '#f1f3f6',
//   enable_publishing: false,
//   allow_symbol_change: false,
//   container_id: 'mycontainer',
// })
