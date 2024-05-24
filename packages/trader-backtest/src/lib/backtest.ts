import { OHLCV, Position, Trade } from '@code9/trader-core'
import * as E from 'fp-ts/Either'
import { pipe } from 'fp-ts/function'
import * as RE from 'fp-ts/ReaderEither'
import * as RNEA from 'fp-ts/ReadonlyNonEmptyArray'

export type CalculatePricesProps = {
  base: number
  quote: number
  price: number
  transactionCostPercentage: number
}

export type CalculatePricesResult = {
  base: number
  quote: number
}

export const isntBaseZeroE = E.fromPredicate<CalculatePricesProps, Error>(
  (b) =>
    !(b.base === 0),
  () =>
    new Error('Base is zero'),
)

export const isntQuoteZeroE = E.fromPredicate<CalculatePricesProps, Error>(
  (b) =>
    !(b.quote === 0),
  () =>
    new Error('Quote is zero'),
)

export const isntPriceZeroE = E.fromPredicate<CalculatePricesProps, Error>(
  (b) =>
    !(b.price === 0),
  () =>
    new Error('Price is zero'),
)

export const isntTransactionCostPercentageNegativeE = E.fromPredicate<CalculatePricesProps, Error>(
  (b) =>
    !(b.transactionCostPercentage < 0),
  () =>
    new Error('transactionCostPercentage is negative'),
)

export const calculateBuyPrices: RE.ReaderEither<CalculatePricesProps, Error, CalculatePricesResult> = (a) =>
  pipe(
    a,
    isntQuoteZeroE,
    E.chain(isntPriceZeroE),
    E.chain(isntTransactionCostPercentageNegativeE),
    E.map((b) => {
      let transactionPrice = b.quote / b.price
      transactionPrice -= (b.transactionCostPercentage / 100) * transactionPrice

      return {
        base: transactionPrice,
        quote: 0,
      }
    }),
  )

export const calculateSellPrices: RE.ReaderEither<CalculatePricesProps, Error, CalculatePricesResult> = (a) =>
  pipe(
    a,
    isntBaseZeroE,
    E.chain(isntPriceZeroE),
    E.chain(isntTransactionCostPercentageNegativeE),
    E.map((b) => {
      let transactionPrice = b.base * b.price
      transactionPrice -= (b.transactionCostPercentage / 100) * transactionPrice

      return {
        base: 0,
        quote: transactionPrice,
      }
    }),
  )

export type BacktestState = {
  currentPosition: Position
  currentTrade: Maybe<Trade>
  currentAmountBase: number
  currentAmountQuote: number
  trades: Array<Trade>
}

export type BacktestOperationProps = {
  state: E.Either<Error, BacktestState>
  bar: OHLCV
  transactionCostPercentage: number
}

export type BacktestOperation = (props: BacktestOperationProps) => E.Either<Error, BacktestState>

const positionIsntBuyE = E.fromPredicate<BacktestState, Error>(
  (state) =>
    state.currentPosition !== Position.Buy,
  () =>
    new Error('Cannot buy when currentPosition is Buy.'),
)

/**
 * Simulates a buy operation in a backtesting environment using fp-ts.
 *
 * @param {BacktestOperationProps} props - The properties for the buy operation.
 * @returns {E.Either<Error, BacktestState>} Either containing the updated backtesting state or an error.
 */
export const buy: BacktestOperation = ({ state, bar, transactionCostPercentage }) =>
  pipe(
    state,
    E.chain(positionIsntBuyE),
    E.chain((currentState) =>
      pipe(
        calculateBuyPrices({
          base: currentState.currentAmountBase,
          quote: currentState.currentAmountQuote,
          price: bar.close,
          transactionCostPercentage,
        }),
        E.map((prices) => {
          return {
            ...currentState,
            currentPosition: Position.Buy,
            currentAmountBase: prices.base,
            currentAmountQuote: prices.quote,
            currentTrade: {
              startDate: bar.time,
              endDate: 0,
              startPrice: bar.close,
              endPrice: 0,
              base: prices.base,
              quote: prices.quote,
            },
            trades: currentState.trades,
          }
        }),
      )),
  )

// const positionHasTradeE = E.fromPredicate<BacktestState, Error>(
//   (state) =>
//     state.currentTrade !== undefined,
//   () =>
//     new Error(`Cannot sell when currentTrade is not defined`),
// )
const positionHasTradeE = E.fromPredicate<BacktestState, BacktestState & { currentTrade: Trade }, Error>(
  (state): state is BacktestState & { currentTrade: Trade } =>
    state.currentTrade !== undefined,
  () =>
    new Error(`Cannot sell when currentTrade is not defined`),
)

/**
 * Simulates a sell operation in a backtesting environment using fp-ts.
 *
 * @param {BacktestOperationProps} props - The properties for the sell operation.
 * @returns {E.Either<Error, BacktestState>} Either containing the updated backtesting state or an error.
 */
export const sell: BacktestOperation = ({ state, bar, transactionCostPercentage }) =>
  pipe(
    state,
    E.chain(positionHasTradeE),
    E.chain((currentState) =>
      pipe(
        calculateSellPrices({
          base: currentState.currentAmountBase,
          quote: currentState.currentAmountQuote,
          price: bar.close,
          transactionCostPercentage,
        }),
        E.map((prices) => {
          return {
            ...currentState,
            currentPosition: Position.Sell,
            currentAmountBase: prices.base,
            currentAmountQuote: prices.quote,
            currentTrade: undefined,
            trades: [
              ...currentState.trades,
              {
                startDate: currentState.currentTrade.startDate,
                startPrice: currentState.currentTrade.startPrice,
                endDate: bar.time,
                endPrice: bar.close,
                base: prices.base,
                quote: prices.quote,
              },
            ],
          }
        }),
      )),
  )

/**
 * Simulates a hold operation in a backtesting environment using fp-ts.
 *
 * @param {BacktestOperationProps} props - The properties for the hold operation.
 * @returns {E.Either<Error, BacktestState>} The updated backtesting state after the hold operation.
 */
export const hold: BacktestOperation = ({ state }) =>
  state

const backtestOperations = {
  [Position.Buy]: buy,
  [Position.Sell]: sell,
  [Position.Hold]: hold,
}

export const backtest = (
  bars: Array<OHLCV>,
  positions: RNEA.ReadonlyNonEmptyArray<Position>,
  initialAmount: number,
  transactionCostPercentage = 0,
): E.Either<Error, Array<Trade>> =>
  pipe(
    positions,
    RNEA.reduceWithIndex<Position, E.Either<Error, BacktestState>>(
      E.right({
        currentPosition: Position.Hold,
        currentTrade: undefined,
        currentAmountBase: 0,
        currentAmountQuote: initialAmount,
        trades: [],
      }),
      (index, state, position) =>
        pipe(
          E.fromNullable(new Error(`Invalid position: ${position}`))(bars[index]),
          E.chain((bar) => {
            const operation = backtestOperations[position]
            return pipe(
              operation({ state, bar, transactionCostPercentage }),
              E.map((newState) => {
                return { ...newState, currentPosition: position }
              }),
            )
          }),
        ),
    ),
    E.map((resultState) =>
      resultState.trades),
  )

export const logTrades = (trades: Array<Trade>, initialAmount: number): void => {
  if (trades.length === 0) {
    console.log(`No trades found`)
    return
  }

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

    const startDate = new Date(trade.startDate).toISOString()
    const endDate = new Date(trade.endDate).toISOString()
    const { startPrice, endPrice } = trade
    const percentageDifference = (-(100 - (quote / previousQuote) * 100)).toFixed(2)

    console.log(`${startDate} (${startPrice}) -> ${endDate} (${endPrice}) ${quote} (${percentageDifference}%)`)
  }
}

export const logProfitPercentages = (trades: Array<Trade>, initialAmount: number): void => {
  const lastTrade = trades[trades.length - 1]
  // TODO: Unit Test.
  if (typeof lastTrade === 'undefined') {
    throw new Error(`No trade at index ${trades.length - 1}`)
  }

  const profitPercentage = (lastTrade.quote / initialAmount) * 100
  const profitPercentages = trades.map((entry) =>
    100 - (entry.endPrice / entry.startPrice) * 100)
  const winningTrades = profitPercentages.filter((entry) =>
    entry > 0)
  const profitabilityPercentage = (winningTrades.length / trades.length) * 100

  console.log(`Profit Percentage: ${profitPercentage.toFixed(2)}%`)
  console.log(`Profitability Percentage: ${profitabilityPercentage.toFixed(2)}%`)
}
