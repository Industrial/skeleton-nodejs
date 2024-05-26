import { describe, expect, test } from 'bun:test'

import { simulatedAnnealing } from './simulated-annealing.ts'

describe('Simulated Annealing', () => {
  test('Should return initial state when minimumTemperature is reached', () => {
    const result = simulatedAnnealing({
      initialState: 8,
      minimumTemperature: 0.001,
      maximumTemperature: 15,
      state: (previousState) =>
        previousState + 0.5,
      energy: (state) =>
        Math.abs(state * state - 16),
      temperature: (previousTemperature) =>
        previousTemperature - 0.001,
    })

    // Result is -4 or 4 so abs it.
    expect(Math.abs(Math.round(result))).toBe(8)
  })

  test('Should return initial state when maximumIterations is reached', () => {
    let iterations = 0

    simulatedAnnealing({
      initialState: Math.random() * 16,
      minimumTemperature: 0.001,
      maximumTemperature: 15,
      state: (previousState) => {
        iterations += 1
        return previousState + (Math.random() - 0.5)
      },
      energy: (state) =>
        Math.abs(state * state - 16),
      temperature: (previousTemperature) =>
        previousTemperature - 0.001,
      maximumIterations: 10,
    })

    expect(iterations).toBe(10)
  })
})
