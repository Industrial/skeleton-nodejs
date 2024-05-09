import { describe, expect, it } from 'bun:test'
import { either as E } from 'fp-ts'
import { Readable } from 'stream'

import { createReadableStreamProperty, streamToString } from './child_process.ts'

describe('child_process', () => {
  describe('streamToString', () => {
    describe('When called with a stream', () => {
      describe('When the stream emits an error', () => {
        it('should return an error', async () => {
          const stream = new Readable({
            read() {
              this.emit('error', new Error('error'))
            },
          })
          const result = await streamToString(stream)()
          expect(result).toEqual(E.left(new Error('error')))
        })
      })
      describe('When the stream emits no data', () => {
        it('should return an empty string', async () => {
          const stream = new Readable({
            read() {
              this.push(null)
            },
          })
          const result = await streamToString(stream)()
          expect(result).toEqual(E.right(''))
        })
      })
      describe('When the stream emits data', () => {
        it('should return a string', async () => {
          const stream = new Readable({
            read() {
              this.push('hello')
              this.push('world')
              this.push(null)
            },
          })
          const result = await streamToString(stream)()
          expect(result).toEqual(E.right('helloworld'))
        })
      })
    })
  })

  describe('createStreamProperty', () => {
    describe('When called with a stream', () => {
      describe('When the stream emits an error', () => {
        it('should return an error', async () => {
          const stream = new ReadableStream({
            start(controller) {
              controller.error(new Error('error'))
            },
          })
          const property = createReadableStreamProperty(stream)
          return new Promise<void>((resolve) => {
            property.subscribe({
              next: () => {
                expect(property.get()).toEqual(E.left(new Error('error')))
                resolve()
              },
            })
          })
        })
      })

      describe('When the stream emits no data', () => {
        it('should return an empty buffer', async () => {
          const stream = new ReadableStream({
            start(controller) {
              controller.close()
            },
          })
          const property = createReadableStreamProperty(stream)
          property.subscribe({
            next: () => {
              expect(property.get()).toEqual(E.right(new Uint8Array()))
            },
          })
        })
      })

      describe('When the stream emits data', () => {
        it('should return a buffer', async () => {
          const stream = new ReadableStream({
            start(controller) {
              // eslint-disable-next-line @typescript-eslint/no-floating-promises
              (async () => {
                controller.enqueue(new TextEncoder().encode('hello'))
                await new Promise((resolve) =>
                  // eslint-disable-next-line no-promise-executor-return
                  setTimeout(resolve, 100))
                controller.enqueue(new TextEncoder().encode('world'))
                await new Promise((resolve) =>
                  // eslint-disable-next-line no-promise-executor-return
                  setTimeout(resolve, 100))
                controller.close()
              })()
            },
          })

          const property = createReadableStreamProperty(stream)

          let counter = 0
          return new Promise<void>((resolve) => {
            property.subscribe({
              next: () => {
                if (counter === 0) {
                  expect(property.get()).toEqual(E.right(new TextEncoder().encode('hello')))
                  counter += 1
                } else {
                  expect(property.get()).toEqual(E.right(new TextEncoder().encode('world')))
                  resolve()
                }
              },
            })
          })
        })
      })
    })
  })
})

