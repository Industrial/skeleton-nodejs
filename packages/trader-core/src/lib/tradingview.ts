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

export const createGetBatchBars = (tv: TV, symbolInfo: TVSymbolInfo, timeframe: Timeframe, limit: number) =>
  async (batchFrom: Date, batchTo: Date): Promise<Array<OHLCV>> =>
    new Promise((resolve, reject) => {
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
    })

export const fetchBarsInBatches = async (
  tv: TV,
  symbolInfo: TVSymbolInfo,
  timeframe: Timeframe,
  countBack: number,
  limit: number,
): Promise<Array<OHLCV>> => {
  const getBatchBars = createGetBatchBars(tv, symbolInfo, timeframe, limit)

  const fetchBatch = async (
    currentCountBack: number,
    currentFrom: Date,
    currentTo: Date,
    bars: Array<OHLCV> = [],
  ): Promise<Array<OHLCV>> => {
    if (currentCountBack <= 0) {
      return bars
    }

    const batchBars = await getBatchBars(currentFrom, currentTo)

    if (batchBars.length === 0) {
      return bars
    }

    const [firstBar] = batchBars
    if (typeof firstBar === 'undefined') {
      return bars
    }

    // Adding to the front to maintain correct order
    const newBars = [...batchBars, ...bars]

    const newTo = subtract(timeframe, new Date(firstBar.time), 1)
    const newFrom = subtract(timeframe, newTo, limit)

    return fetchBatch(currentCountBack - limit, newFrom, newTo, newBars)
  }

  const to = start(timeframe, new Date(Date.now()))
  const from = subtract(timeframe, to, limit)

  return fetchBatch(countBack, from, to)
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
