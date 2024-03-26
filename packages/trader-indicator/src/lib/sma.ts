import { average } from '@code9/array'

// Function that returns the Simple Moving Average.
export const sma = (length: number, values: Array<number>): Array<number> =>
  values.slice(length - 1).map((_, index) =>
    average(values.slice(index, index + length)))
