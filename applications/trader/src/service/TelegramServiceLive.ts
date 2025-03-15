import { Effect, PubSub } from 'effect'
import type { TelegramMessageEvent } from '../domain/telegram/TelegramMessageEvent'
import type { TelegramError } from './TelegramService'

// TODO: Not sure why we are picking 10 here.
export const MESSAGE_PUBSUB_CAPACITY = 10

/**
 * Publishes messages to subscribers
 */
export const messagePubSub = PubSub.bounded<TelegramMessageEvent>(
  MESSAGE_PUBSUB_CAPACITY,
)

/**
 * Sends a message to a Telegram chat
 * @param chatId The ID of the chat to send the message to
 * @param text The text of the message to send
 * @returns Effect<void, TelegramError>
 */
export const sendMessage = (
  chatId: number | string,
  text: string,
): Effect.Effect<void, TelegramError, never> =>
  // biome-ignore lint/correctness/useYield: <explanation>
  Effect.gen(function* (_) {
    // Implementation will be added later
    console.log(`Sending message to chat ${chatId}: ${text}`)
  })

/**
 * Starts the Telegram service and begins listening for updates
 * @returns Effect<never, TelegramError>
 */
// TODO: Change the return signature
export const start = (): Effect.Effect<void, never, never> =>
  // biome-ignore lint/correctness/useYield: <explanation>
  Effect.gen(function* (_) {
    // Implementation will be added later
    console.log('Telegram Service started')
  })
