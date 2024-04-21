import { getRandomNumber } from '@code9/number'
import { strategySimulatedAnnealing } from '@code9/simulated-annealing'
import { backtest, logProfitPercentages, logTrades } from '@code9/trader-backtest'
import * as localstorage from '@code9/trader-core'
import { fetchBarsInBatches, getSymbol, getSymbolInfo, getTradingView, log, Maybe, millisecondsUntilNextTimeframe, normalizePositions, start, StrategyOptionsBounds, Timeframe, toMs, Trade } from '@code9/trader-core'
import Chart from 'chartjs'
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

const BT_TIMEFRAME: Timeframe = '15m'
const BT_COUNTBACK = 3000
const BT_TRANSACTION_COST_PERCENTAGE = 0.25
const BT_INITIAL_QUOTE = 1

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

const symbol = getSymbol()
const optionsKey = `${symbol}_${BT_TIMEFRAME}_options`
const tradesKey = `${symbol}_${BT_TIMEFRAME}_trades`

const tv = getTradingView()

const loadOptions = (): Maybe<RSISMAAnnealingState> => {
  let options: Maybe<RSISMAAnnealingState>
  const storedOptions = localstorage.get(optionsKey)
  if (storedOptions) {
    options = JSON.parse(String(storedOptions)) as RSISMAAnnealingState
  }
  return options
}

const saveOptions = (options: RSISMAAnnealingState): void => {
  localstorage.set(optionsKey, JSON.stringify(options))
}

const loadTrades = (): Array<Trade> => {
  let trades: Array<Trade> = []
  const storedTrades = localstorage.get(tradesKey)
  if (storedTrades) {
    trades = JSON.parse(String(storedTrades)) as Array<Trade>
  }
  return trades
}

const saveTrades = (trades: Array<Trade>): void => {
  localstorage.set(tradesKey, JSON.stringify(trades))
}

export const chartTrades = (trades: Array<Trade>, initialAmount: number): void => {
  if (trades.length === 0) {
    log.info(`No trades found`)
    return
  }

  const dates: Array<string> = []
  const percentages: Array<string> = []

  const quotes = [initialAmount].concat(trades.map((trade) =>
    trade.quote))

  for (let index = 1; index < quotes.length; index += 1) {
    const previousQuote = quotes[index - 1]
    // TODO: Unit Test.
    if (typeof previousQuote === 'undefined') {
      throw new Error(`No quote at index ${index}`)
    }

    const quote = quotes[index]
    // TODO: Unit Test.
    if (typeof quote === 'undefined') {
      throw new Error(`No quote at index ${index}`)
    }

    // -1 since we are looping through quotes which is one longer then trades.
    const trade = trades[index - 1]

    // TODO: Unit Test.
    if (typeof trade === 'undefined') {
      throw new Error(`No trade at index ${index}`)
    }

    // const startDate = new Date(trade.startDate).toISOString()
    const endDate = new Date(trade.endDate).toISOString()
    // const { startPrice, endPrice } = trade
    const percentageDifference = (-(100 - (quote / previousQuote) * 100)).toFixed(2)

    dates.push(endDate)
    percentages.push(percentageDifference)

    // log.info(`${startDate} (${startPrice}) -> ${endDate} (${endPrice}) ${quote} (${percentageDifference}%)`)
  }

  const id = 'mychart123'

  const divElement: HTMLDivElement = document.querySelector(`#${id}`) ?? document.createElement('div')
  divElement.id = id
  divElement.style.width = '1000px'
  divElement.style.height = '1000px'
  divElement.style.zIndex = '999999'
  divElement.style.position = 'absolute'
  divElement.innerHTML = ''

  const canvasElement = document.createElement('canvas')
  canvasElement.id = 'myChart'
  canvasElement.style.width = '100%'
  canvasElement.style.height = '100%'

  const chart = new Chart(canvasElement, {
    type: 'bar',
    data: {
      labels: dates,
      datasets: [
        {
          label: 'Percentages',
          data: percentages,
          borderWidth: 1,
        },
        {
          label: 'Quotes',
          data: quotes,
          borderWidth: 1,
        },
      ],
    },
    options: {
      scales: {
        y: {
          beginAtZero: true,
        },
      },
    },
  })

  divElement.appendChild(canvasElement)

  if (!document.querySelector(`#${id}`)) {
    document.body.appendChild(divElement)
  }

  // const csv = [
  //   'Date,Percentage,Quote',
  // ]
  // dates.forEach((date, index) => {
  //   csv.push(`${date},${percentages[index]},${quotes[index]}`)
  // })
  //
  // log.info(csv.join('\n'))
}

const main = async (): Promise<void> => {
  const symbolInfo = await getSymbolInfo(tv, symbol)
  log.info('symbolInfo', symbolInfo)

  let options = loadOptions()
  let trades = loadTrades()

  const work = async () => {
    try {
      const bars = await fetchBarsInBatches(tv, symbolInfo, BT_TIMEFRAME, BT_COUNTBACK, 1000)
      const maximumAnnealingIterations = BT_COUNTBACK - bars.length + 1000

      log.info(`Transaction Cost Percentage: ${BT_TRANSACTION_COST_PERCENTAGE}%`)
      log.info(`Bars: ${bars.length}`)
      log.info(`Timeframe: ${BT_TIMEFRAME}`)
      log.info(`Initial Quote: ${BT_INITIAL_QUOTE}`)
      log.info(`Maximum Annealing Iterations: ${maximumAnnealingIterations}`)

      if (!options || trades.length === 0) {
        log.info(`Starting annealing...`)

        // eslint-disable-next-line require-atomic-updates
        options = await strategySimulatedAnnealing<RSISMAAnnealingState>(
          bars,
          maximumAnnealingIterations,
          BT_INITIAL_QUOTE,
          BT_TRANSACTION_COST_PERCENTAGE,
          bounds,
          getInitialState,
          rsiSmaStrategy,
        )
        log.debug('options', options)
        const positions = RNEA.fromArray(normalizePositions(rsiSmaStrategy(options)(bars)))
        if (O.isNone(positions)) {
          throw new Error('No positions found')
        }

        const newTrades = backtest(
          bars,
          positions.value,
          BT_INITIAL_QUOTE,
          BT_TRANSACTION_COST_PERCENTAGE,
        )
        if (E.isLeft(newTrades)) {
          throw newTrades.left
        }
        // eslint-disable-next-line require-atomic-updates
        trades = newTrades.right
        log.info('Options: ', options)
      } else {
        log.info(`Updaning annealing...`)

        const lastTrade: Maybe<Trade> = trades[trades.length - 1]
        log.debug('lastTrade', lastTrade)
        if (typeof lastTrade === 'undefined') {
          throw new Error(`No last trade`)
        }

        const newOptions = await strategySimulatedAnnealing(
          bars,
          maximumAnnealingIterations,
          BT_INITIAL_QUOTE,
          BT_TRANSACTION_COST_PERCENTAGE,
          bounds,
          getInitialState,
          rsiSmaStrategy,
        )
        log.debug('newOptions', newOptions)

        const positions = RNEA.fromArray(normalizePositions(rsiSmaStrategy(options)(bars)))
        log.debug('positions', positions)
        if (O.isNone(positions)) {
          throw new Error('No positions found')
        }

        const newTrades = backtest(bars, positions.value, BT_INITIAL_QUOTE, BT_TRANSACTION_COST_PERCENTAGE)
        log.debug('newTrades', newTrades)
        if (E.isLeft(newTrades)) {
          throw newTrades.left
        }
        const newLastTrade: Maybe<Trade> = newTrades.right[newTrades.right.length - 1]

        if (typeof newLastTrade !== 'undefined') {
          if (newLastTrade.quote > lastTrade.quote) {
            // eslint-disable-next-line require-atomic-updates
            options = newOptions
            // eslint-disable-next-line require-atomic-updates
            trades = newTrades.right
          }
        }
      }

      log.info('Options: ', options)
      saveOptions(options)
      saveTrades(trades)

      logTrades(trades, BT_INITIAL_QUOTE)
      logProfitPercentages(trades, BT_INITIAL_QUOTE)
      // chartTrades(trades, BT_INITIAL_QUOTE)

      const lastTrade = trades[trades.length - 1]
      if (typeof lastTrade === 'undefined') {
        throw new Error(`No trades`)
      }

      const now = new Date(Date.now())

      const startOfCurrentTimeframe = start(BT_TIMEFRAME, new Date(Date.now()))
      log.info(startOfCurrentTimeframe.toISOString(), 'startOfCurrentTimeframe')

      const startDate = new Date(lastTrade.startDate)
      log.info(startDate.toISOString(), 'startDate')

      const endDate = new Date(lastTrade.endDate)
      log.info(endDate.toISOString(), 'endDate')

      if (startOfCurrentTimeframe === startDate) {
        log.info(`${now.toISOString()} BUY ${symbol} ${lastTrade.startPrice}`)
      } else if (startOfCurrentTimeframe === endDate) {
        log.info(`${now.toISOString()} BUY ${symbol} ${lastTrade.startPrice}`)
      }
    } catch (error: unknown) {
      console.error(error)
    }
  }

  const now = new Date()
  const timeout = millisecondsUntilNextTimeframe(BT_TIMEFRAME, now)
  log.info(`Waiting for ${timeout / 1000 / 60} minutes...`)

  setTimeout(async () => {
    const interval = toMs(BT_TIMEFRAME)
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

