import { backtest } from '@code9/trader-backtest'
import { normalizePositions, OHLCV, Strategy, StrategyOptions, StrategyOptionsBounds } from '@code9/trader-core'
import { Effect as Fx, pipe } from 'effect'

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

/**
 * Performs the Simulated Annealing optimization algorithm to find a state with
 * a lower energy.
 *
 * Simulated Annealing is a probabilistic technique for approximating the global
 * optimum of a given function.
 * This function iteratively improves the state by probabilistically accepting
 * worse states, which allows it to escape local minima.
 *
 * @template State - Type representing the state in the solution space.
 *
 * @param {State} initialState - The starting state from which the algorithm
 * begins.
 * @param {number} minimumTemperature - The stopping condition for the
 * temperature; the algorithm halts if the temperature goes below this value.
 * @param {number} maximumTemperature - The initial temperature at the start of
 * the algorithm.
 * @param {StateFunction<State>} state - A function that generates a new
 * neighboring state from the current state.
 * @param {TemperatureFunction} temperature - A function that calculates the new
 * temperature at each iteration.
 * @param {EnergyFunction<State>} energy - A function that computes the energy
 * (or cost) of a given state; lower energy indicates a better state.
 * @param {number} [maximumIterations=Infinity] - Optional parameter to limit
 * the number of iterations.
 *
 * @returns {State} - The state with the lowest energy found.
 */
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

/**
 * Extends the Simulated Annealing optimization algorithm with caching to avoid
 * recalculating the energy of previously encountered states.
 *
 * This variant of the Simulated Annealing algorithm uses a cache to store
 * energies of previously visited states. This helps in avoiding
 * recomputation and can guide the algorithm to new unexplored states.
 *
 * @template State - Type representing the state in the solution space.
 *
 * @param {State} initialState - The starting state from which the algorithm begins.
 * @param {number} minimumTemperature - The stopping condition for the
 * temperature; the algorithm halts if the temperature goes below this value.
 * @param {number} maximumTemperature - The initial temperature at the start of
 * the algorithm.
 * @param {StateFunction<State>} state - A function that generates a new
 * neighboring state from the current state.
 * @param {TemperatureFunction} temperature - A function that calculates the new
 * temperature at each iteration.
 * @param {EnergyFunction<State>} energy - A function that computes the energy
 * (or cost) of a given state; lower energy indicates a better state.
 * @param {number} [maximumIterations=Infinity] - Optional parameter to limit
 * the number of iterations.
 *
 * @returns {State} - The state with the lowest energy found after utilizing
 * caching.
 */
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

/**
 * Calculates the next temperature for the simulated annealing process.
 *
 * This function reduces the temperature linearly, facilitating the gradual
 * cooling process integral to the simulated annealing algorithm. The rate
 * of reduction is constant.
 *
 * @param {number} previousTemperature - The temperature from the previous
 * iteration.
 *
 * @returns {number} - The new temperature for the current iteration.
 */
export const calculateStrategyTemperature = (previousTemperature: number) =>
  previousTemperature - 0.01

/**
 * Generates a new neighboring state by mutating one randomly selected parameter
 * within its defined bounds.
 *
 * This function is part of the simulated annealing process for strategy
 * optimization. By creating slight variations in the current state, it explores
 * different configurations within the given bounds. The mutation is constrained
 * by the bounds defined for each parameter, ensuring the new state is valid.
 *
 * @template Options - Type representing the strategy options in the solution space.
 *
 * @param {StrategyOptionsBounds<Options>} bounds - The bounds within which each
 * parameter of the state is allowed to vary.
 *
 * @returns {(state: Options) => Options} - A function that takes the current
 * state and returns a new mutated state.
 *
 * @throws {Error} - If the state contains an invalid key.
 */
export const calculateStrategyState = <Options extends StrategyOptions>(
  bounds: StrategyOptionsBounds<Options>,
) =>
    (state: Options) => {
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
    }

/**
 * Computes the energy (cost) of a given strategy state by backtesting the
 * strategy on provided OHLCV bars and calculating the resulting performance.
 *
 * This function integrates the strategy, backtests it, and normalizes the
 * positions. It returns the negative of the final quote to align with the
 * energy minimization approach of simulated annealing. States resulting in
 * invalid positions or fewer than 5 trades are considered high energy
 * (undesirable).
 *
 * @template Options - Type representing the strategy options.
 *
 * @param {Strategy<Options>} strategy - The trading strategy to be optimized.
 * @param {Array<OHLCV>} bars - The historical OHLCV data used for backtesting.
 * @param {number} initialQuote - The initial amount of quote currency.
 * @param {number} transactionCostPercentage - The transaction cost as a
 * percentage.
 *
 * @returns {(state: Options) => number} - A function that takes a strategy
 * state and returns the computed energy (negative of the final quote).
 *
 * @throws {Error} - If the state generates invalid positions or the trades are
 * invalid.
 */
export const calculateStrategyEnergy = <Options extends StrategyOptions>(
  strategy: Strategy<Options>,
  bars: Array<OHLCV>,
  initialQuote: number,
  transactionCostPercentage: number,
) =>
    (state: Options) => {
      const positions = pipe(
        bars,
        strategy(state),
        normalizePositions,
      )
      if (positions.length === 0) {
        throw new Error('Invalid positions')
      }

      const trades = Fx.runSync(backtest(
        bars,
        positions,
        initialQuote,
        transactionCostPercentage,
      ))

      const lastTrade = trades[trades.length - 1]
      if (typeof lastTrade === 'undefined') {
        return initialQuote
      }

      const amountOfTrades = trades.length
      if (amountOfTrades < 5) {
        return initialQuote
      }

      const result = -lastTrade.quote
      return result
    }

/**
 * Optimizes a given trading strategy using the Simulated Annealing algorithm
 * with caching.
 *
 * This function applies the simulated annealing optimization with caching to
 * find the optimal set of strategy options. It uses historical OHLCV data to
 * backtest the strategy and evaluate its performance by minimizing energy,
 * which represents the negative of the final quote value after trades.
 *
 * @template Options - Type representing the strategy options.
 *
 * @param {Array<OHLCV>} bars - The historical OHLCV data used for backtesting
 * the strategy.
 * @param {number} maximumIterations - The maximum number of iterations for the
 * annealing process.
 * @param {number} initialQuote - The initial amount of quote currency.
 * @param {number} transactionCostPercentage - The transaction cost as a
 * percentage.
 * @param {StrategyOptionsBounds<Options>} bounds - The bounds within which each
 * parameter of the strategy options is allowed to vary.
 * @param {() => Options} getInitialState - A function that provides the initial
 * strategy options state.
 * @param {Strategy<Options>} strategy - The trading strategy to be optimized.
 *
 * @returns {Promise<Options>} - The optimized strategy options found by the
 * simulated annealing process.
 */
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
    temperature: (previousTemperature) =>
      calculateStrategyTemperature(previousTemperature),
    state: (state: Options) =>
      calculateStrategyState(bounds)(state),
    energy: (state: Options) =>
      calculateStrategyEnergy(strategy, bars, initialQuote, transactionCostPercentage)(state),
    maximumIterations,
  })

  return annealingState
}
