import { describe, expect, it } from 'bun:test'
import { Effect, Either } from 'effect'
import {
  InvalidTelegramMessageEventError,
  createTelegramMessageEvent,
} from './TelegramMessageEvent'

describe('TelegramMessageEvent', () => {
  // Valid parameters for testing
  const validParams = {
    messageId: 12345,
    chatId: 67890,
    userId: 54321,
    text: 'Hello, world!',
    timestamp: 1609459200, // 2021-01-01 00:00:00 UTC
    metadata: {
      isBot: false,
      username: 'testuser',
    },
  }

  describe('createTelegramMessageEvent', () => {
    it('should create a valid telegram message event', () => {
      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(validParams)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual(validParams)
      }
    })

    it('should create a valid telegram message event without optional metadata', () => {
      const paramsWithoutMetadata = {
        messageId: 12345,
        chatId: 67890,
        userId: 54321,
        text: 'Hello, world!',
        timestamp: 1609459200,
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(paramsWithoutMetadata)),
      )

      expect(Either.isRight(result)).toBe(true)
      if (Either.isRight(result)) {
        expect(result.right).toEqual(paramsWithoutMetadata)
      }
    })

    it('should fail with non-positive message ID', () => {
      const invalidParams = {
        ...validParams,
        messageId: 0,
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'Message ID must be positive',
        )
      }
    })

    it('should fail with non-positive chat ID', () => {
      const invalidParams = {
        ...validParams,
        chatId: -1,
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'Chat ID must be positive',
        )
      }
    })

    it('should fail with non-positive user ID', () => {
      const invalidParams = {
        ...validParams,
        userId: 0,
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'User ID must be positive',
        )
      }
    })

    it('should fail with non-positive timestamp', () => {
      const invalidParams = {
        ...validParams,
        timestamp: -1,
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'Timestamp must be positive',
        )
      }
    })

    it('should fail with empty text', () => {
      const invalidParams = {
        ...validParams,
        text: '',
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'Message text cannot be empty',
        )
      }
    })

    it('should fail with whitespace-only text', () => {
      const invalidParams = {
        ...validParams,
        text: '   ',
      }

      const result = Effect.runSync(
        Effect.either(createTelegramMessageEvent(invalidParams)),
      )

      expect(Either.isLeft(result)).toBe(true)
      if (Either.isLeft(result)) {
        expect(result.left).toBeInstanceOf(InvalidTelegramMessageEventError)
        expect((result.left as InvalidTelegramMessageEventError).message).toBe(
          'Message text cannot be empty',
        )
      }
    })
  })
})
