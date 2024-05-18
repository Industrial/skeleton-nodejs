import { isUndefined } from '@code9/undefined'

export type Alpha = (length: number) => number

export const emaAlpha = (length: number): number =>
  2 / (length + 1)

// Function that returns the Exponential Moving Average.
export const ema = (length: number, values: Array<number>, alpha: Alpha = emaAlpha): Array<number> => {
  if (values.length === 0) {
    return []
  }

  const [firstValue] = values

  if (isUndefined(firstValue)) {
    return []
  }

  return values.reduce(
    (previousValue, currentValue, currentIndex) => {
      if (currentIndex === 0) {
        return previousValue
      }

      const lastPreviousValue = previousValue[previousValue.length - 1]

      if (isUndefined(lastPreviousValue)) {
        return previousValue
      }

      return previousValue.concat(alpha(length) * currentValue + (1 - alpha(length)) * lastPreviousValue)
    },
    [firstValue],
  )
}
