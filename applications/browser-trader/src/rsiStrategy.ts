import { Position, Strategy } from '@code9/trader-core'
import { hasCrossedBoundaryDownwardsAtIndex, hasCrossedBoundaryUpwardsAtIndex, rsi, sma } from '@code9/trader-indicator'

export type RSIStrategyProps = {
  rsiLength: number
  rsiLowerLimit: number
  rsiUpperLimit: number
}
export const rsiStrategy: Strategy<RSIStrategyProps> =
  ({ rsiLength, rsiLowerLimit, rsiUpperLimit }) =>
    (bars) => {
      const rsiValues = rsi(
        rsiLength,
        bars.map((b) =>
          b.close),
      )

      const positions = rsiValues.map((_currentValue, index) => {
        if (hasCrossedBoundaryUpwardsAtIndex(rsiValues, rsiLowerLimit, index - 1)) {
          return Position.Buy
        } else if (hasCrossedBoundaryDownwardsAtIndex(rsiValues, rsiUpperLimit, index - 1)) {
          return Position.Sell
        }
        return Position.Hold
      })

      return positions
    }

export type RSISMAStrategyProps = {
  rsiLength: number
  rsiLowerLimit: number
  rsiUpperLimit: number
  smaLength: number
}
export const rsiSmaStrategy: Strategy<RSISMAStrategyProps> =
  ({ rsiLength, rsiLowerLimit, rsiUpperLimit, smaLength }) =>
    (bars) => {
      const rsiValues = rsi(
        rsiLength,
        bars.map((b) =>
          b.close),
      )
      const smaValues = sma(smaLength, rsiValues)

      const positions = smaValues.map((_currentValue, index) => {
        if (hasCrossedBoundaryUpwardsAtIndex(smaValues, rsiLowerLimit, index - 1)) {
          return Position.Buy
        } else if (hasCrossedBoundaryDownwardsAtIndex(smaValues, rsiUpperLimit, index - 1)) {
          return Position.Sell
        }
        return Position.Hold
      })

      return positions
    }
