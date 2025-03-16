import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import {
  InvalidSignalError,
  SignalDirection,
  createBuySignal,
  createNoSignal,
  createSellSignal,
  createSignal,
} from './Signal'

describe('Signal', () => {
  const timestamp = Date.now()
  const price = 50000

  describe('createSignal', () => {
    it('should create a valid signal', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price,
            timestamp,
            strength: 0.8,
            metadata: { indicator: 'MACD', value: 0.5 },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Buy,
          price,
          timestamp,
          strength: 0.8,
          metadata: { indicator: 'MACD', value: 0.5 },
        })
      }
    })

    it('should create a valid signal without optional parameters', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price,
            timestamp,
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Buy,
          price,
          timestamp,
        })
      }
    })

    it('should fail with negative price', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price: -100,
            timestamp,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Price must be non-negative',
        )
      }
    })

    it('should fail with invalid timestamp', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price,
            timestamp: -1,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Timestamp must be positive',
        )
      }
    })

    it('should fail with strength below 0', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price,
            timestamp,
            strength: -0.1,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Strength must be between 0 and 1',
        )
      }
    })

    it('should fail with strength above 1', () => {
      const result = Effect.runSync(
        Effect.either(
          createSignal({
            direction: SignalDirection.Buy,
            price,
            timestamp,
            strength: 1.1,
          }),
        ),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Strength must be between 0 and 1',
        )
      }
    })
  })

  describe('createBuySignal', () => {
    it('should create a valid buy signal', () => {
      const result = Effect.runSync(
        Effect.either(
          createBuySignal(price, timestamp, {
            strength: 0.8,
            metadata: { indicator: 'MACD', value: 0.5 },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Buy,
          price,
          timestamp,
          strength: 0.8,
          metadata: { indicator: 'MACD', value: 0.5 },
        })
      }
    })

    it('should create a valid buy signal without optional parameters', () => {
      const result = Effect.runSync(
        Effect.either(createBuySignal(price, timestamp)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Buy,
          price,
          timestamp,
        })
      }
    })

    it('should fail with negative price', () => {
      const result = Effect.runSync(
        Effect.either(createBuySignal(-100, timestamp)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Price must be non-negative',
        )
      }
    })
  })

  describe('createSellSignal', () => {
    it('should create a valid sell signal', () => {
      const result = Effect.runSync(
        Effect.either(
          createSellSignal(price, timestamp, {
            strength: 0.8,
            metadata: { indicator: 'MACD', value: -0.5 },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Sell,
          price,
          timestamp,
          strength: 0.8,
          metadata: { indicator: 'MACD', value: -0.5 },
        })
      }
    })

    it('should create a valid sell signal without optional parameters', () => {
      const result = Effect.runSync(
        Effect.either(createSellSignal(price, timestamp)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.Sell,
          price,
          timestamp,
        })
      }
    })

    it('should fail with negative price', () => {
      const result = Effect.runSync(
        Effect.either(createSellSignal(-100, timestamp)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Price must be non-negative',
        )
      }
    })
  })

  describe('createNoSignal', () => {
    it('should create a valid no-action signal', () => {
      const result = Effect.runSync(
        Effect.either(
          createNoSignal(price, timestamp, {
            metadata: { reason: 'No trend detected' },
          }),
        ),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.None,
          price,
          timestamp,
          strength: 0,
          metadata: { reason: 'No trend detected' },
        })
      }
    })

    it('should create a valid no-action signal without optional parameters', () => {
      const result = Effect.runSync(
        Effect.either(createNoSignal(price, timestamp)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual({
          direction: SignalDirection.None,
          price,
          timestamp,
          strength: 0,
        })
      }
    })

    it('should fail with negative price', () => {
      const result = Effect.runSync(
        Effect.either(createNoSignal(-100, timestamp)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidSignalError)
        expect((result.left as InvalidSignalError).message).toBe(
          'Price must be non-negative',
        )
      }
    })
  })
})
