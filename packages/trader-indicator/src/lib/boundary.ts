import type { Maybe } from '@code9/trader-core'

export const hasCrossedBoundaryUpwardsAtIndex = (values: Array<number>, boundary: number, index: number): boolean => {
  const previousValue: Maybe<number> = values[index - 1]
  const currentValue: Maybe<number> = values[index]
  if (!previousValue || !currentValue) {
    return false
  }
  // return previousValue <= boundary && currentValue > boundary
  return previousValue < boundary && currentValue >= boundary
}

export const hasCrossedBoundaryDownwardsAtIndex = (values: Array<number>, boundary: number, index: number): boolean => {
  const previousValue: Maybe<number> = values[index - 1]
  const currentValue: Maybe<number> = values[index]
  if (!previousValue || !currentValue) {
    return false
  }
  // return previousValue >= boundary && currentValue < boundary
  return previousValue > boundary && currentValue <= boundary
}
