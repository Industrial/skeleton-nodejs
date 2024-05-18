import { isNull } from '@code9/null'
import { isUndefined } from '@code9/undefined'
import { newProperty, type Observer, type Property } from '@frp-ts/core'
import * as E from 'fp-ts/Either'
import * as TE from 'fp-ts/TaskEither'
import { Readable } from 'stream'

export const createHandleError = (writer: WritableStreamDefaultWriter<Uint8Array>) =>
  (error: Error): void => {
    // Made this promise 'floating' because there is no way to handle the error.
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    writer.abort(error)
  }

export const createHandleData = (writer: WritableStreamDefaultWriter<Uint8Array>) =>
  async (chunk: Buffer | null | undefined): Promise<void> => {
    if (isNull(chunk) || isUndefined(chunk)) {
      await writer.close()
      return
    }
    const uint8Array = new Uint8Array(chunk.buffer, chunk.byteOffset, chunk.byteLength)
    await writer.write(uint8Array)
  }

export const createHandleEnd = (writer: WritableStreamDefaultWriter<Uint8Array>) =>
  async (): Promise<void> => {
    await writer.close()
  }

export const nodeReadableToReadableStream = (nodeStream: Readable): ReadableStream => {
  const transformStream = new TransformStream<Uint8Array, Uint8Array>()
  const writer = transformStream.writable.getWriter()

  nodeStream.on('error', createHandleError(writer))
  nodeStream.on('data', createHandleData(writer))
  nodeStream.on('end', createHandleData(writer))

  return transformStream.readable
}

export const streamToString = (stream: Readable): TE.TaskEither<Error, string> =>
  TE.tryCatch(
    async () => {
      let data = ''
      for await (const chunk of stream) {
        data += chunk
      }
      return data
    },
    E.toError,
  )

export const createReadableStreamProperty = <T>(stream: ReadableStream): Property<E.Either<Error, Uint8Array>> => {
  const reader = stream.getReader()
  let currentValue: E.Either<Error, Uint8Array> = E.right(new Uint8Array())
  let subscribed = false
  const handleError = (error: Error) => {
    currentValue = E.left(error)
  }
  const get = () =>
    currentValue
  const subscribe = (observer: Observer<E.Either<Error, Uint8Array>>) => {
    (async () => {
      try {
        subscribed = true
        // eslint-disable-next-line @stylistic/js/max-len
        // eslint-disable-next-line no-constant-condition, @typescript-eslint/no-unnecessary-condition, no-unmodified-loop-condition
        while (true) {
          // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
          if (!subscribed) {
            // observer.next(E.right(new Uint8Array()))
            break
          }
          const { done, value } = await reader.read()
          if (done) {
            break
          }
          currentValue = E.right(value)
          observer.next(currentValue)
        }
      } catch (error: unknown) {
        handleError(error as Error)
        observer.next(currentValue)
      }
    })().catch(handleError)
    return {
      unsubscribe: () => {
        subscribed = false
      },
    }
  }

  // @ts-expect-error why only number
  return newProperty(get, subscribe)
}
