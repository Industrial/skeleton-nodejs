import { backtest } from '@code9/trader-backtest'
import { Maybe, normalizePositions, OHLCV, Strategy, StrategyOptions, StrategyOptionsBounds, Trade } from '@code9/trader-core'
import * as E from 'fp-ts/Either'
import * as O from 'fp-ts/Option'
import * as RNEA from 'fp-ts/ReadonlyNonEmptyArray'

export type StateFunction<State> = (previousState: State) => State
export type EnergyFunction<State> = (state: State) => number
export type TemperatureFunction = (previousTemperature: number) => number

export type SimulatedAnnealingProps<State> = {
  initialState: State
  minimumTemperature: number
  maximumTemperature: number
  state: StateFunction<State>
  energy: EnergyFunction<State>
  temperature: TemperatureFunction
  maximumIterations?: number
}

export const simulatedAnnealing = <State>({
  initialState,
  minimumTemperature,
  maximumTemperature,
  state,
  temperature,
  energy,
  maximumIterations = Infinity,
}: SimulatedAnnealingProps<State>): State => {
  let currentTemperature: number = maximumTemperature
  let lastState: State = initialState
  let lastEnergy: number = energy(lastState)
  let bestState: State = lastState
  let bestEnergy: number = lastEnergy

  let currentState: State
  let currentEnergy: number

  let currentIteration = maximumIterations
  while (currentIteration > 0 && currentTemperature > minimumTemperature) {
    currentState = state(lastState)
    currentEnergy = energy(currentState)

    if (currentEnergy > lastEnergy) {
      lastState = currentState
      lastEnergy = currentEnergy
    } else if (Math.random() <= Math.exp(-(currentEnergy - lastEnergy) / currentTemperature)) {
      lastState = currentState
      lastEnergy = currentEnergy
    }

    if (bestEnergy > lastEnergy) {
      bestState = lastState
      bestEnergy = lastEnergy
    }

    currentTemperature = temperature(currentTemperature)
    currentIteration -= 1
  }

  return bestState
}

export const cachedSimulatedAnnealing = <State>({
  initialState,
  minimumTemperature,
  maximumTemperature,
  state,
  temperature,
  energy,
  maximumIterations,
}: SimulatedAnnealingProps<State>): State => {
  const energyCache = new Map<number, State>()

  let attempts = 0
  const cachingEnergy = (newState: State): number => {
    attempts += 1
    const newEnergy = energy(newState)
    if (energyCache.has(newEnergy)) {
      // Since the lower the energy the better, increasing it as the attempts
      // increase will deincentivize the algorithm to look at this spot.
      const value = attempts * 0.001
      return value
    }
    energyCache.set(newEnergy, newState)
    return newEnergy
  }

  return simulatedAnnealing({
    initialState,
    minimumTemperature,
    maximumTemperature,
    state,
    temperature,
    energy: cachingEnergy,
    maximumIterations,
  })
}

export const strategySimulatedAnnealing = async <Options extends StrategyOptions>(
  bars: Array<OHLCV>,
  maximumIterations: number,
  initialQuote: number,
  transactionCostPercentage: number,
  bounds: StrategyOptionsBounds<Options>,
  getInitialState: () => Options,
  strategy: Strategy<Options>,
): Promise<Options> => {
  const annealingState = cachedSimulatedAnnealing<Options>({
    initialState: getInitialState(),
    maximumTemperature: 15,
    minimumTemperature: 0.01,
    temperature: (previousTemperature) => {
      return previousTemperature - 0.01
    },
    state: (state: Options) => {
      const keys = Object.keys(state)
      const randomKey = keys[Math.floor(Math.random() * keys.length)] as keyof typeof state
      const mutation = Math.random() > 0.5 ? 1 : -1
      const keyBounds = bounds[randomKey]
      const keyState = state[randomKey]
      if (!keyState) {
        throw new Error(`Invalid state`)
      }
      let newValue = keyState + mutation
      if (newValue < keyBounds[0] || newValue > keyBounds[1]) {
        newValue = keyState - mutation
      }
      if (newValue < keyBounds[0] || newValue > keyBounds[1]) {
        newValue = keyState
      }
      const newState = {
        ...state,
        [randomKey]: newValue,
      }
      return newState
    },
    energy: (state: Options) => {
      const positions = RNEA.fromArray(normalizePositions(strategy(state)(bars)))
      if (O.isNone(positions)) {
        throw new Error(`Invalid positions`)
      }

      const trades = backtest(bars, positions.value, initialQuote, transactionCostPercentage)
      if (E.isLeft(trades)) {
        throw trades.left
      }

      const lastTrade: Maybe<Trade> = trades.right[trades.right.length - 1]
      if (typeof lastTrade === 'undefined') {
        return initialQuote
      }

      const amountOfTrades = trades.right.length
      if (amountOfTrades < 5) {
        return initialQuote
      }

      const result = -lastTrade.quote
      return result
    },
    maximumIterations,
  })

  return annealingState
}
