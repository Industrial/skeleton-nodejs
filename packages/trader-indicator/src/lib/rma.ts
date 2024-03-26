import { ema } from './ema.ts'

const rmaAlpha = (length: number): number =>
  1 / length

// Function that returns the RMA; the Expontential Moving Average with alpha = 1 / length.
export const rma = (length: number, values: Array<number>): Array<number> =>
  ema(length, values, rmaAlpha)
