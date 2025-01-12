import { PredicateError, fromPredicate } from '@code9/effect'
import type { OHLCV, Trade } from '@code9/trader-core'
import { Position } from '@code9/trader-core'
import { Effect as Fx, pipe } from 'effect'

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

/**
 * Validates that the base amount is non-zero.
 *
 * @param {CalculatePricesProps} a - The properties for calculating prices.
 * @returns {Fx.Effect<boolean, PredicateError, CalculatePricesProps>} Effect
 * containing the validation result or a PredicateError.
 */
export const isntBaseZeroE = fromPredicate(
  (a: CalculatePricesProps): a is CalculatePricesProps => a.base !== 0,
  () => new PredicateError({ message: 'Base is zero' }),
)

/**
 * Validates that the quote amount is non-zero.
 *
 * @param {CalculatePricesProps} a - The properties for calculating prices.
 * @returns {Fx.Effect<boolean, PredicateError, CalculatePricesProps>} Effect
 * containing the validation result or a PredicateError.
 */
export const isntQuoteZeroE = fromPredicate(
  (a: CalculatePricesProps): a is CalculatePricesProps => a.quote !== 0,
  () => new PredicateError({ message: 'Quote is zero' }),
)

/**
 * Validates that the price is non-zero.
 *
 * @param {CalculatePricesProps} a - The properties for calculating prices.
 * @returns {Fx.Effect<boolean, PredicateError, CalculatePricesProps>} Effect
 * containing the validation result or a PredicateError.
 */
export const isntPriceZeroE = fromPredicate(
  (a: CalculatePricesProps): a is CalculatePricesProps => a.price !== 0,
  () => new PredicateError({ message: 'Price is zero' }),
)

/**
 * Validates that the transaction cost percentage is non-negative.
 *
 * @param {CalculatePricesProps} a - The properties for calculating prices.
 * @returns {Fx.Effect<boolean, PredicateError, CalculatePricesProps>} Effect
 * containing the validation result or a PredicateError.
 */
export const isntTransactionCostPercentageNegativeE = fromPredicate(
  (a: CalculatePricesProps): a is CalculatePricesProps =>
    a.transactionCostPercentage >= 0,
  () =>
    new PredicateError({ message: 'transactionCostPercentage is negative' }),
)

/**
 * Calculates the resulting prices for a buy operation.
 *
 * This function validates the provided properties to ensure non-zero and
 * non-negative values, then calculates the transaction price for the buy
 * operation considering the transaction cost percentage.
 *
 * @param {CalculatePricesProps} props - The properties for calculating prices.
 * @returns {Fx.Effect<CalculatePricesResult, PredicateError, unknown>} Effect
 * containing the calculated prices or a PredicateError.
 */
export const calculateBuyPrices = <E, R>(
  props: CalculatePricesProps,
): Fx.Effect<CalculatePricesResult, E | PredicateError, R> =>
  pipe(
    Fx.Do,
    Fx.bind('validateQuote', () => isntQuoteZeroE(props)),
    Fx.bind('validatePrice', ({ validateQuote }) =>
      isntPriceZeroE(validateQuote),
    ),
    Fx.bind('validateTransactionCostPercentage', ({ validatePrice }) =>
      isntTransactionCostPercentageNegativeE(validatePrice),
    ),
    Fx.map(({ validateTransactionCostPercentage }) => {
      let transactionPrice =
        validateTransactionCostPercentage.quote /
        validateTransactionCostPercentage.price
      transactionPrice -=
        (validateTransactionCostPercentage.transactionCostPercentage / 100) *
        transactionPrice

      return {
        base: transactionPrice,
        quote: 0,
      }
    }),
  ) as Fx.Effect<CalculatePricesResult, E | PredicateError, R>

/**
 * Calculates the resulting prices for a sell operation.
 *
 * This function validates the provided properties to ensure non-zero and
 * non-negative values, then calculates the transaction price for the sell
 * operation considering the transaction cost percentage.
 *
 * @param {CalculatePricesProps} props - The properties for calculating prices.
 * @returns {Fx.Effect<CalculatePricesResult, PredicateError, unknown>} Effect
 * containing the calculated prices or a PredicateError.
 */
export const calculateSellPrices = <E, R>(
  props: CalculatePricesProps,
): Fx.Effect<CalculatePricesResult, E | PredicateError, R> =>
  pipe(
    Fx.Do,
    Fx.bind('validateBase', () => isntBaseZeroE(props)),
    Fx.bind('validatePrice', ({ validateBase }) =>
      isntPriceZeroE(validateBase),
    ),
    Fx.bind('validateTransactionCostPercentage', ({ validatePrice }) =>
      isntTransactionCostPercentageNegativeE(validatePrice),
    ),
    Fx.map(({ validateTransactionCostPercentage }) => {
      let transactionPrice =
        validateTransactionCostPercentage.base *
        validateTransactionCostPercentage.price
      transactionPrice -=
        (validateTransactionCostPercentage.transactionCostPercentage / 100) *
        transactionPrice

      return {
        base: 0,
        quote: transactionPrice,
      }
    }),
  ) as Fx.Effect<CalculatePricesResult, E | PredicateError, R>

export type BacktestState = {
  currentPosition: Position
  currentTrade: Trade | undefined
  currentAmountBase: number
  currentAmountQuote: number
  trades: Array<Trade>
}

export type BacktestOperationProps<E, R> = {
  state: Fx.Effect<BacktestState, E, R>
  bar: OHLCV
  transactionCostPercentage: number
}

export type BacktestOperation = <A extends BacktestState, E, R>(
  props: BacktestOperationProps<E, R>,
) => Fx.Effect<A, E, R>

/**
 * Validates that the current position is not Position.Buy.
 *
 * @param {BacktestState} state - The current state of the backtest.
 * @returns {Fx.Effect<boolean, PredicateError, BacktestState>} Effect
 * containing the validation result or a PredicateError if the position is
 * Position.Buy.
 */
const positionIsntBuyE = fromPredicate(
  (state: BacktestState): state is BacktestState =>
    state.currentPosition !== Position.Buy,
  () =>
    new PredicateError({ message: 'Cannot buy when currentPosition is Buy.' }),
)

/**
 * Simulates a buy operation in a backtesting environment using fp-ts.
 *
 * This function first validates that the current position is not already Buy.
 * Then, it calculates the new prices for the buy operation considering the
 * transaction cost percentage. Finally, it updates the backtest state to
 * reflect the buy operation.
 *
 * @template E - The type of error that may be thrown.
 * @template R - The type of result that may be returned.
 * @param {BacktestOperationProps} props - The properties for the buy operation.
 * @param {Fx.Effect<BacktestState>} props.state - The current state of the
 * backtest.
 * @param {OHLCV} props.bar - The current "Open-High-Low-Close-Volume" data.
 * @param {number} props.transactionCostPercentage - The transaction cost as a
 * percentage.
 * @returns {Fx.Effect<BacktestState, E | PredicateError, R>} The updated
 * backtesting state or a PredicateError.
 */
export const buy = <E, R>({
  state,
  bar,
  transactionCostPercentage,
}: BacktestOperationProps<E, R>): Fx.Effect<
  BacktestState,
  E | PredicateError,
  R
> =>
  pipe(
    state,
    Fx.flatMap(positionIsntBuyE),
    Fx.flatMap((currentState) =>
      pipe(
        calculateBuyPrices({
          base: currentState.currentAmountBase,
          quote: currentState.currentAmountQuote,
          price: bar.close,
          transactionCostPercentage,
        }),
        Fx.map((prices) => {
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
      ),
    ),
  ) as Fx.Effect<BacktestState, E | PredicateError, R>

const positionHasTradeE = fromPredicate(
  (state: BacktestState): state is BacktestState & { currentTrade: Trade } =>
    state.currentTrade !== undefined,
  () =>
    new PredicateError({
      message: 'Cannot sell when currentTrade is not defined',
    }),
)

/**
 * Validates that there is an existing trade in the current state.
 *
 * This function checks if `currentTrade` is defined in the given `BacktestState`.
 * It returns an `Fx.Effect` that either contains the original state
 * (if `currentTrade` is defined) or a `PredicateError` (if `currentTrade`
 * is not defined).
 *
 * @param {BacktestState} state - The current state of the backtest.
 * @returns {Fx.Effect<boolean, PredicateError, BacktestState>} An Effect
 * containing the validation result or a PredicateError.
 */
export const sell = <E, R>({
  state,
  bar,
  transactionCostPercentage,
}: BacktestOperationProps<E, R>): Fx.Effect<
  BacktestState,
  E | PredicateError,
  R
> =>
  pipe(
    state,
    Fx.flatMap(positionHasTradeE),
    Fx.flatMap((currentState) =>
      pipe(
        calculateSellPrices({
          base: currentState.currentAmountBase,
          quote: currentState.currentAmountQuote,
          price: bar.close,
          transactionCostPercentage,
        }),
        Fx.map((prices) => {
          return {
            ...currentState,
            currentPosition: Position.Sell,
            currentAmountBase: prices.base,
            currentAmountQuote: prices.quote,
            currentTrade: undefined,
            trades: [
              ...currentState.trades,
              {
                startDate: currentState.currentTrade?.startDate,
                startPrice: currentState.currentTrade?.startPrice,
                endDate: bar.time,
                endPrice: bar.close,
                base: prices.base,
                quote: prices.quote,
              },
            ],
          }
        }),
      ),
    ),
  ) as Fx.Effect<BacktestState, E | PredicateError, R>

/**
 * Simulates a hold operation in a backtesting environment.
 *
 * This function does not modify the state, effectively simulating a "hold"
 * position where no trade action is taken.
 *
 * @template E - The type of error that may be thrown.
 * @template R - The type of result that may be returned.
 * @param {BacktestOperationProps} props - The properties for the hold operation.
 * @param {Fx.Effect<BacktestState>} props.state - The current state of the
 * backtest.
 * @returns {Fx.Effect<BacktestState, E | PredicateError, R>} The unmodified
 * backtesting state.
 */
export const hold = <E, R>({
  state,
}: BacktestOperationProps<E, R>): Fx.Effect<
  BacktestState,
  E | PredicateError,
  R
> => state

const backtestOperations = {
  [Position.Buy]: buy,
  [Position.Sell]: sell,
  [Position.Hold]: hold,
}

/**
 * Executes a backtest operation using an array of OHLCV data and corresponding
 * positions.
 * This function processes each position and its corresponding bar, updating the
 * backtest state
 * through a composition of effects.
 *
 * @param {Array<OHLCV>} bars - Array of OHLCV (Open-High-Low-Close-Volume)
 * data.
 * @param {Array<Position>} positions - Array of positions to simulate (Buy,
 * Sell, Hold).
 * @param {number} initialAmount - Initial amount of quote currency available
 * for backtesting.
 * @param {number} [transactionCostPercentage=0] - Transaction cost as a
 * percentage of each trade.
 * @returns {Fx.Effect<Array<Trade>, E | PredicateError, R>} Effect containing either
 * an error or the array of executed trades.
 *
 * @example
 * // Simulate a backtest with the given bars and positions
 * const result = backtest(bars, positions, 1000, 0.1);
 */
export const backtest = <E, R>(
  bars: Array<OHLCV>,
  positions: Array<Position>,
  initialAmount: number,
  transactionCostPercentage = 0,
): Fx.Effect<Array<Trade>, E | PredicateError, R> =>
  pipe(
    positions,
    Fx.reduce(
      {
        currentPosition: Position.Hold,
        currentTrade: undefined,
        currentAmountBase: 0,
        currentAmountQuote: initialAmount,
        trades: [],
      } as BacktestState,
      (state, position, index) =>
        pipe(
          Fx.fromNullable(bars[index]),
          Fx.flatMap((bar) =>
            pipe(
              backtestOperations[position]({
                state: Fx.succeed(state),
                bar,
                transactionCostPercentage,
              }),
              Fx.map((newState) => {
                return { ...newState, currentPosition: position }
              }),
            ),
          ),
        ) as Fx.Effect<BacktestState, E | PredicateError, R>,
    ),
    Fx.map((resultState) => resultState.trades),
  )

/**
 * Logs the details of each trade, including start and end dates, prices,
 * amounts, and percentage differences. It is intended for tracking the
 * performance of backtested trades.
 *
 * @param {Array<Trade>} trades - Array of executed trades to be logged.
 * @param {number} initialAmount - The initial amount of quote currency before
 * any trades were executed.
 *
 * @example
 * // Log the details of the trades
 * logTrades(trades, 1000);
 */
export const logTrades = (
  trades: Array<Trade>,
  initialAmount: number,
): void => {
  if (trades.length === 0) {
    console.log('No trades found')
    return
  }

  const quotes = [initialAmount].concat(trades.map((trade) => trade.quote))

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
    const percentageDifference = (-(
      100 -
      (quote / previousQuote) * 100
    )).toFixed(2)

    console.log(
      `${startDate} (${startPrice}) -> ${endDate} (${endPrice}) ${quote} (${percentageDifference}%)`,
    )
  }
}

/**
 * Logs the profit percentage for the last trade and the overall profitability
 * percentage of all trades. The profitability percentage is calculated as the
 * percentage of trades that ended with a profit.
 *
 * @param {Array<Trade>} trades - Array of executed trades.
 * @param {number} initialAmount - The initial amount of quote currency before
 * any trades were executed.
 *
 * @example
 * // Log the profit percentages of the trades
 * logProfitPercentages(trades, 1000);
 */
export const logProfitPercentages = (
  trades: Array<Trade>,
  initialAmount: number,
): void => {
  const lastTrade = trades[trades.length - 1]
  // TODO: Unit Test.
  if (typeof lastTrade === 'undefined') {
    throw new Error(`No trade at index ${trades.length - 1}`)
  }

  const profitPercentage = (lastTrade.quote / initialAmount) * 100
  const profitPercentages = trades.map(
    (entry) => 100 - (entry.endPrice / entry.startPrice) * 100,
  )
  const winningTrades = profitPercentages.filter((entry) => entry > 0)
  const profitabilityPercentage = (winningTrades.length / trades.length) * 100

  console.log(`Profit Percentage: ${profitPercentage.toFixed(2)}%`)
  console.log(
    `Profitability Percentage: ${profitabilityPercentage.toFixed(2)}%`,
  )
}
