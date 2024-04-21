import * as E from 'fp-ts/Either'
import * as FN from 'fp-ts/function'
import * as TE from 'fp-ts/TaskEither'

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

export const getTradingView = (): TV => {
  const [tvKey] = Object.keys(globalThis).filter((x) =>
    x.startsWith('tradingview_'))
  const tv = globalThis[tvKey]
  if (!tv) {
    throw new Error('TradingView Not Found')
  }
  return tv
}

export const getSymbolInfo = async (tv: TV, symbolName: string): Promise<TVSymbolInfo> =>
  await new Promise((resolve, reject) => {
    tv.datafeed.resolveSymbol(symbolName, resolve, reject)
  })

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
