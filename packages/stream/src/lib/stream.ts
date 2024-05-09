import { newProperty, type Property } from '@frp-ts/core'
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
  async (chunk: Buffer): Promise<void> => {
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

export const createReadableStreamProperty = (stream: ReadableStream): Property<E.Either<Error, Uint8Array>> => {
  const reader = stream.getReader()
  let currentValue: E.Either<Error, Uint8Array> = E.right(new Uint8Array())
  const handleError = (error: Error) => {
    currentValue = E.left(error)
  }
  return newProperty(
    () =>
      currentValue,
    (observer) => {
      (async () => {
        try {
          // eslint-disable-next-line no-constant-condition, @typescript-eslint/no-unnecessary-condition
          while (true) {
            const { done, value } = await reader.read()
            if (done) {
              break
            }
            currentValue = E.right(value)
            observer.next(0)
          }
        } catch (error: unknown) {
          handleError(error as Error)
          observer.next(0)
        }
      })().catch(handleError)
      return {
        unsubscribe: () => {
          // Unsubscribe logic here, if needed
        },
      }
    },
  )
}
