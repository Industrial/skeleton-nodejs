/**
 * Base structure for candlestick data with price and volume information
 */
export interface CandlestickStructure {
  /** Unix timestamp in milliseconds */
  timestamp: number
  /** Opening price of the period */
  open: number
  /** Highest price of the period */
  high: number
  /** Lowest price of the period */
  low: number
  /** Closing price of the period */
  close: number
  /** Trading volume of the period */
  volume: number
}
