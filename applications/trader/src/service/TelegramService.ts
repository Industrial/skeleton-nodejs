import { Context, Data, type Effect, Layer, type PubSub } from 'effect'
import type { TelegramMessageEvent } from '../domain/telegram/TelegramMessageEvent'
import * as TelegramServiceLive from './TelegramServiceLive'

export class TelegramError extends Data.TaggedError('TelegramError')<{
  readonly message: string
  readonly cause?: unknown
}> {}

export interface TelegramServiceType {
  /**
   * Publishes messages to subscribers
   */
  messagePubSub: Effect.Effect<
    PubSub.PubSub<TelegramMessageEvent>,
    never,
    never
  >

  /**
   * Sends a message to a Telegram chat
   * @param chatId The ID of the chat to send the message to
   * @param text The text of the message to send
   * @returns Effect<void, TelegramError>
   */
  sendMessage(
    chatId: number | string,
    text: string,
  ): Effect.Effect<void, TelegramError, never>

  /**
   * Starts the Telegram service and begins listening for updates
   * @returns Effect<never, TelegramError>
   */
  // TODO: Change this signature
  start(): Effect.Effect<void, never, never>
}

export class TelegramService extends Context.Tag('TelegramService')<
  TelegramServiceType,
  TelegramServiceType
>() {
  static Live = Layer.succeed(this, TelegramServiceLive)
}
