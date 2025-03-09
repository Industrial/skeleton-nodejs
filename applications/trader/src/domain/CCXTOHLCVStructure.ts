/**
 * CCXT library OHLCV data structure
 * [timestamp, open, high, low, close, volume]
 */
export type CCXTOHLCVStructure = readonly [
  number,
  number,
  number,
  number,
  number,
  number,
]
