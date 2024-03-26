import { rma } from './rma.ts'

export const returns = (values: Array<number>): Array<number> =>
  values
    .map((value, index) => {
      const lastValue = values[index - 1] ?? 0
      return index === 0 ? 0 : value - lastValue
    })
    .slice(1)

// Function that returns the RSI.
export const rsi = (length: number, values: Array<number>): Array<number> => {
  const changes = returns(values)

  const ups = rma(
    length,
    changes.map((value) =>
      Math.max(value, 0)),
  )

  const downs = rma(
    length,
    changes.map((value) =>
      -Math.min(value, 0)),
  )

  const rsis = downs.map((value, index) => {
    const upValue = ups[index] ?? 0
    return value === 0 ? 100 : upValue === 0 ? 0 : 100 - 100 / (1 + upValue / value)
  })

  return rsis
}
