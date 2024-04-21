// Import { createStream } from 'rotating-file-stream'
// import { Logger } from 'tslog'

// Silly
// Trace
// Debug
// Info
// Warn
// Error
// Fatal
// export const log = new Logger({
//   minLevel: 1,
//   type: 'pretty',
//   stylePrettyLogs: false,
// })

export const log = {
  info: (...args: Array<unknown>): void => {
    console.log(...args)
  },
  debug: (...args: Array<unknown>): void => {
    console.log(...args)
  },
  error: (...args: Array<unknown>): void => {
    console.error(...args)
  },
}

// Const logStream = createStream('log/request.log', {
//   Size: '10M',
//   Interval: '1d',
//   Compress: 'gzip',
// })

// Log.attachTransport((logObject) => {
//   LogStream.write(JSON.stringify(logObject) + '\n')
// })
