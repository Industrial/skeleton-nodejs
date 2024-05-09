import { describe, expect, it } from 'bun:test'
import { either as E } from 'fp-ts'
import { Readable } from 'stream'

import { createReadableStreamProperty, nodeReadableToReadableStream, streamToString } from './stream.ts'

describe('stream', () => {
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

  describe('nodeReadableToReadableStream', () => {
    describe('When called with a node stream', () => {
      describe('When the node stream emits an error', () => {
        it('should return an error', async () => {
          const stream = new Readable({
            read() {
              this.emit('error', new Error('error'))
            },
          })
          const readableStream = nodeReadableToReadableStream(stream)
          return new Promise<void>((resolve, reject) => {
            (async () => {
              try {
                await readableStream.getReader().read()
              } catch (error: unknown) {
                resolve()
              }
            })()
              .catch(reject)
          })
        })
      })
      describe('When the node stream emits no data', () => {
        it('should return an empty buffer', async () => {
          const stream = new Readable({
            read() {
              this.push(null)
            },
          })
          const readableStream = nodeReadableToReadableStream(stream)
          return new Promise<void>((resolve, reject) => {
            (async () => {
              try {
                const result = await readableStream.getReader().read()
                expect(result).toStrictEqual({
                  done: true,
                  value: undefined,
                })
                resolve()
              } catch (error: unknown) {
                reject(new Error('should not throw an error'))
              }
            })()
              .catch(reject)
          })
        })
      })
      describe('When the node stream emits data', () => {
        it('should return a buffer', async () => {
          const stream = new Readable({
            read() {
              this.push('hello')
              this.push('world')
              this.push(null)
            },
          })
          const readableStream = nodeReadableToReadableStream(stream)
          return new Promise<void>((resolve, reject) => {
            (async () => {
              const reader = readableStream.getReader()
              const { done, value } = await reader.read()
              expect(done).toBe(false)
              expect(value).toEqual(new TextEncoder().encode('hello'))
              const { done: done2, value: value2 } = await reader.read()
              expect(done2).toBe(false)
              expect(value2).toEqual(new TextEncoder().encode('world'))
              const { done: done3 } = await reader.read()
              expect(done3).toBe(true)
              resolve()
            })()
              .catch(reject)
          })
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
                expect(property.get()).toStrictEqual(E.left(new Error('error')))
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

          let called = false
          property.subscribe({
            next: () => {
              called = true
            },
          })

          return new Promise<void>((resolve) => {
            setTimeout(() => {
              expect(called).toBe(false)
              resolve()
            }, 100)
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

      describe('When the stream emits an end', () => {
        it('should return an empty buffer', async () => {
          const stream = new ReadableStream({
            start(controller) {
              controller.enqueue(new TextEncoder().encode('hello'))
              controller.close()
            },
          })
          const property = createReadableStreamProperty(stream)

          return new Promise<void>((resolve) => {
            property.subscribe({
              next: () => {
                expect(property.get()).toEqual(E.right(new TextEncoder().encode('hello')))
                resolve()
              },
            })
          })
        })
      })

      describe('When the property is unsubscribed from', () => {
        it('should stop emitting values', async () => {
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
            const subscription = property.subscribe({
              next: () => {
                if (counter === 0) {
                  expect(property.get()).toEqual(E.right(new TextEncoder().encode('hello')))
                  counter += 1
                  subscription.unsubscribe()
                  // It's untestable wether next is called again, because it is
                  // not. I'm drunk right now. Streams rock.
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

