import { OHLCV } from './ohlcv.ts'
import { start, subtract, Timeframe, toMs } from './timeframe.ts'

// TODO: Type
type TV = unknown
type SymbolInfo = unknown

// const iframe = document.querySelector(`#${tvKey}`)
// const chartWidget = iframe.contentWindow.tradingViewApi._chartWidgetCollection.getAll()[0]
// this._innerWindow().tradingViewApi
// tradingViewApi.chart(t)
// this._iFrame.contentWindow
export const getTradingView = (): TV => {
  const [tvKey] = Object.keys(globalThis).filter((x) => x.startsWith('tradingview_'))
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  const tv = globalThis[tvKey]
  if (!tv) {
    throw new Error('TradingView Not Found')
  }

  return tv
}

export const getSymbolInfo = async (tv: TV, symbolName: string): Promise<SymbolInfo> =>
  // eslint-disable-next-line require-atomic-updates
   await new Promise((resolve, reject) => {
    // @ts-expect-error untyped
    tv.datafeed.resolveSymbol(symbolName, resolve, reject)
  })

export const fetchBarsInBatches = async (
tv: TV,
  symbolInfo: SymbolInfo,
  timeframe: Timeframe,
  countBack: number,
  limit: number,
): Promise<Array<OHLCV>> => {
  const bars: Array<OHLCV> = []
  const to = start(timeframe, new Date(Date.now()))
  const from = subtract(timeframe, to, limit)

  let currentCountBack = countBack
  let currentFrom = from
  let currentTo = to

  while (currentCountBack > 0) {
    // eslint-disable-next-line @typescript-eslint/no-loop-func
    const batchBars: Array<OHLCV> = await new Promise((resolve, reject) => {
      // @ts-expect-error type
      tv.datafeed.getBars(
symbolInfo,
        String(toMs(timeframe) / 1000 / 60),
        {
          from: currentFrom.valueOf() / 1000,
          to: currentTo.valueOf() / 1000,
          countBack: limit,
        },
        resolve,
        reject,
)
    })

    if (batchBars.length === 0) {
      currentCountBack = 0
      continue
    }

    const [firstBar] = batchBars
    if (typeof firstBar === 'undefined') {
      currentCountBack = 0
      continue
    }

    // Adding to the front to maintain correct order
    bars.unshift(...batchBars)

    // eslint-disable-next-line require-atomic-updates
    currentTo = subtract(timeframe, new Date(firstBar.time), 1)
    // eslint-disable-next-line require-atomic-updates
    currentFrom = subtract(timeframe, currentTo, limit)

    currentCountBack -= limit
  }

  return bars
}

// TODO: Test.
export const getSymbol = (): string => document.title
    .split('/')
    .slice(0, 2)
    .map((entry) => entry.split(' '))
    .map((entry) => entry
        .filter((word) => word !== '')
        .slice(0, 1)
        .pop())
    .join('/')
