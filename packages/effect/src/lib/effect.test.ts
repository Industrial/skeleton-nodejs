import { describe, expect, it } from 'bun:test'
import { Either as E, Effect as Fx, Option as O } from 'effect'

import {
  PredicateError,
  fromEither,
  fromOption,
  fromPredicate,
} from './effect.ts'

describe('When using fromPredicate', () => {
  describe('When the predicate function returns true', () => {
    it('should return a success effect with the value', () => {
      const result = fromPredicate<number, PredicateError, never>(
        (a: number) => a > 0,
        () => new PredicateError({ message: 'Predicate failed' }),
      )
      expect(Fx.runSync(result(1))).toBe(1)
    })
  })

  describe('When the predicate function returns false', () => {
    it('should return a failure effect with the error', () => {
      const result = fromPredicate<number, PredicateError, never>(
        (a: number) => a > 0,
        () => new PredicateError({ message: 'Predicate failed' }),
      )
      expect(() => Fx.runSync(result(-1))).toThrowError(
        new PredicateError({ message: 'Predicate failed' }),
      )
    })
  })
})

describe('When using fromEither', () => {
  describe('When the Either is a Left value', () => {
    it('should return a failure effect with the left value', () => {
      const result = fromEither<number, PredicateError, never>(
        E.left(new PredicateError({ message: 'Predicate failed' })),
      )
      expect(() => Fx.runSync(result)).toThrowError(
        new PredicateError({ message: 'Predicate failed' }),
      )
    })
  })

  describe('When the Either is a Right value', () => {
    it('should return a success effect with the right value', () => {
      const result = fromEither<number, PredicateError, never>(E.right(1))
      expect(Fx.runSync(result)).toBe(1)
    })
  })
})

describe('When using fromOption', () => {
  describe('When the Option is Some', () => {
    it('should return a success effect with the value', () => {
      const result = fromOption<number, PredicateError, never>(
        () => new PredicateError({ message: 'Predicate failed' }),
      )
      expect(Fx.runSync(result(O.some(1)))).toBe(1)
    })
  })

  describe('When the Option is None', () => {
    it('should return a failure effect with the error', () => {
      const result = fromOption<number, PredicateError, never>(
        () => new PredicateError({ message: 'Predicate failed' }),
      )
      expect(() => Fx.runSync(result(O.none()))).toThrowError(
        new PredicateError({ message: 'Predicate failed' }),
      )
    })
  })
})
