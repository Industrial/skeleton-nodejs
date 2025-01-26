import { Readable } from 'node:stream'
import type { ReactDOMServerReadableStream } from 'react-dom/server'

export const concatenate = (streams: Readable[]) =>
  new Readable({
    async read() {
      for (const stream of streams) {
        for await (const chunk of stream) {
          this.push(chunk)
        }
      }
      this.push(null)
    },
  })

export const fromString = (html: string) =>
  new Readable({
    read() {
      this.push(html)
      this.push(null)
    },
  })

export const fromReactDomServerReadableStream = (
  reactStream: ReactDOMServerReadableStream,
): Readable =>
  new Readable({
    read() {
      ;(async () => {
        const reader = reactStream.getReader()
        try {
          while (true) {
            const { done, value } = await reader.read()
            if (done) break
            this.push(value)
          }
        } catch (error: unknown) {
          this.destroy(error as Error)
        } finally {
          this.push(null)
        }
      })()
    },
  })
