import { Data, Effect, Schema } from 'effect'

export class InvalidTelegramMessageEventError extends Data.TaggedError(
  'InvalidTelegramMessageEventError',
)<{
  readonly message: string
}> {}

/**
 * Schema for Telegram message event metadata
 */
export const TelegramMessageEventMetadataSchema = Schema.Record({
  key: Schema.String,
  value: Schema.Unknown,
})

/**
 * Type for validated telegram message event metadata
 */
export type TelegramMessageEventMetadataType = Schema.Schema.Type<
  typeof TelegramMessageEventMetadataSchema
>

/**
 * Schema for Telegram message event data
 */
export const TelegramMessageEventSchema = Schema.Struct({
  /** Unique identifier for the message */
  messageId: Schema.Number,

  /** Chat ID where the message was sent */
  chatId: Schema.Number,

  /** Sender user ID */
  userId: Schema.Number,

  /** Message text content */
  text: Schema.String,

  /** Timestamp when the message was sent (Unix timestamp in seconds) */
  timestamp: Schema.Number,

  /** Optional metadata for additional event information */
  metadata: Schema.optional(TelegramMessageEventMetadataSchema),
})

/**
 * Type for validated Telegram message event
 */
export type TelegramMessageEvent = Schema.Schema.Type<
  typeof TelegramMessageEventSchema
>

/**
 * Creates a validated TelegramMessageEvent
 * @param params Signal parameters
 * @returns Effect containing the validated Signal
 * @throws {InvalidTelegramMessageEventError} When signal parameters are invalid
 */
export const createTelegramMessageEvent = (
  params: TelegramMessageEventInput,
): Effect.Effect<
  TelegramMessageEvent,
  InvalidTelegramMessageEventError,
  never
> =>
  Effect.gen(function* (_) {
    // Validate messageId
    if (params.messageId <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: 'Message ID must be positive',
          }),
        ),
      )
    }

    // Validate chatId
    if (params.chatId <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: 'Chat ID must be positive',
          }),
        ),
      )
    }

    // Validate userId
    if (params.userId <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: 'User ID must be positive',
          }),
        ),
      )
    }

    // Validate timestamp
    if (params.timestamp <= 0) {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: 'Timestamp must be positive',
          }),
        ),
      )
    }

    // Validate text
    if (!params.text || params.text.trim() === '') {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: 'Message text cannot be empty',
          }),
        ),
      )
    }

    // If all validations pass, decode the data
    try {
      return Schema.decodeSync(TelegramMessageEventSchema)(params)
    } catch (error) {
      return yield* _(
        Effect.fail(
          new InvalidTelegramMessageEventError({
            message: `Invalid telegram message event parameters: ${String(error)}`,
          }),
        ),
      )
    }
  })

/**
 * Schema for Telegram message event input parameters
 */
export const TelegramMessageEventInputSchema = Schema.Struct({
  /** Unique identifier for the message */
  messageId: Schema.Number,

  /** Chat ID where the message was sent */
  chatId: Schema.Number,

  /** Sender user ID */
  userId: Schema.Number,

  /** Message text content */
  text: Schema.String,

  /** Timestamp when the message was sent */
  timestamp: Schema.Number,

  /** Optional metadata for additional event information */
  metadata: Schema.optional(TelegramMessageEventMetadataSchema),
})

/**
 * Type for Telegram message event input parameters
 */
export type TelegramMessageEventInput = Schema.Schema.Type<
  typeof TelegramMessageEventInputSchema
>
