import { Cause as C, Effect as Fx, pipe } from 'effect'

import { OHLCV } from './ohlcv.ts'
import { start, subtract, Timeframe, toMs } from './timeframe.ts'

// TODO: Type
export type PromiseResolveFunction<T = unknown> = (value?: T) => void
export type PromiseRejectFunction<T = unknown> = (reason?: T) => void

export type TV = {
  datafeed: {
    resolveSymbol: (symbolName: string, resolve: PromiseResolveFunction, reject: PromiseRejectFunction) => void
    getBars: (
      symbolInfo: TVSymbolInfo,
      resolution: string,
      range: {
        from: number,
        to: number,
        countBack: number
      },
      resolve: PromiseResolveFunction,
      reject: PromiseRejectFunction
    ) => void
  }
}
export type TVResolution = '1' | '1D' | '1M' | '1S' | '1W' | '3' | '3D' | '5' | '15' | '15S' | '30' | '30S' | '60' | '120' | '240' | '480' | '720'
export type TVSymbolInfo = {
  description: string
  exchange: string
  has_daily: boolean
  has_intraday: boolean
  has_seconds: boolean
  logo_urls: Array<string | undefined>
  minmov: number
  name: string
  pricescale: number
  session: string
  supported_resolutions: Array<TVResolution>
  timezone: string
  visible_plots_set: string
}

/**
 * Retrieves the TradingView object from the global environment.
 *
 * This function searches for a global object whose key starts with 'tradingview_'
 * and returns it. If no such object is found, an error is raised.
 *
 * @returns {Fx.Effect<TV, C.NoSuchElementException | E, R>} Effect that
 * describes the process of fetching the TradingView object, which might result
 * in a TV object or an Error.
 */
export const getTradingView = <E, R>(): Fx.Effect<TV, C.NoSuchElementException | E, R> =>
  pipe(
    Fx.fromNullable(Object.keys(globalThis).filter((x) =>
      x.startsWith('tradingview_'))[0]),
    Fx.flatMap((tvKey) =>
      pipe(Fx.fromNullable(globalThis[tvKey as keyof typeof globalThis]))),
  )

/**
 * Retrieves symbol information for the given symbol name.
 *
 * @typeParam T - The type of the information object being returned.
 * @param symbolName - The name of the symbol to retrieve information for.
 * @returns An object of type `T` containing information about the symbol, or `null` if the symbol is not found.
 *
 * @example
 * ```typescript
 * interface StockInfo {
 *   price: number;
 * }
 *
 * const info = getSymbolInfo<StockInfo>('AAPL');
 * if (info) {
 *   console.log(info.price);
 * }
 * ```
 */
export const getSymbolInfo = <E extends Error, R>(tv: TV, symbolName: string): Fx.Effect<TVSymbolInfo, E | Error, R> =>
  pipe(Fx.async<TVSymbolInfo, E | Error, R>((cb) => {
    tv.datafeed.resolveSymbol(
      symbolName,
      (value) => {
        cb(Fx.succeed(value as TVSymbolInfo))
      },
      (reason) => {
        cb(Fx.fail(new Error(reason as string)))
      },
    )
  }))

export const createGetBatchBarsTE = (
  tv: TV,
  symbolInfo: TVSymbolInfo,
  timeframe: Timeframe,
  limit: number,
): ((batchFrom: Date, batchTo: Date) => TE.TaskEither<Error, Array<OHLCV>>) =>
  (batchFrom: Date, batchTo: Date) =>
    TE.tryCatch(
      async () =>
        new Promise<Array<OHLCV>>((resolve, reject) => {
          tv.datafeed.getBars(
            symbolInfo,
            String(toMs(timeframe) / 1000 / 60),
            {
              from: batchFrom.valueOf() / 1000,
              to: batchTo.valueOf() / 1000,
              countBack: limit,
            },
            resolve,
            reject,
          )
        }),
      E.toError,
    )

export const processBatchBars = (
  timeframe: Timeframe,
  limit: number, batchBars: Array<OHLCV>, bars: Array<OHLCV>,
): [Date, Date, Array<OHLCV>] => {
  const newTo = subtract(timeframe, new Date(batchBars[0].time), 1)
  const newFrom = subtract(timeframe, newTo, limit)
  return [
    newFrom,
    newTo,
    [
      ...batchBars,
      ...bars,
    ],
  ]
}

export const fetchBatch = (
  getBatchBarsTE: (batchFrom: Date, batchTo: Date) => TE.TaskEither<Error, Array<OHLCV>>,
  timeframe: Timeframe,
  limit: number,
  currentCountBack: number,
  currentFrom: Date,
  currentTo: Date,
  bars: Array<OHLCV> = [],
): TE.TaskEither<Error, Array<OHLCV>> =>
  currentCountBack <= 0
    ? TE.right(bars)
    : FN.pipe(
      getBatchBarsTE(currentFrom, currentTo),
      TE.chain((batchBars) =>
        batchBars.length === 0
          ? TE.right(bars)
          : fetchBatch(
            getBatchBarsTE,
            timeframe,
            limit,
            currentCountBack - limit,
            ...processBatchBars(timeframe, limit, batchBars, bars),
          )),
    )

export const fetchBarsInBatches = (
  tv: TV,
  symbolInfo: TVSymbolInfo,
  timeframe: Timeframe,
  countBack: number,
  limit: number,
): TE.TaskEither<Error, Array<OHLCV>> => {
  const getBatchBarsTE = createGetBatchBarsTE(tv, symbolInfo, timeframe, limit)
  const to = start(timeframe, new Date(Date.now()))
  const from = subtract(timeframe, to, limit)
  return fetchBatch(getBatchBarsTE, timeframe, limit, countBack, from, to)
}

// TODO: Test.
export const getSymbol = (): string =>
  document.title
    .split('/')
    .slice(0, 2)
    .map((entry) =>
      entry.split(' '))
    .map((entry) =>
      entry
        .filter((word) =>
          word !== '')
        .slice(0, 1)
        .pop())
    .join('/')
