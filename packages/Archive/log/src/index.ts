/* eslint-disable no-console */
// Log formats
export type LogFormat = 'json' | 'pretty'

// Log levels
export type LogLevel =
  | 'silly'
  | 'trace'
  | 'debug'
  | 'info'
  | 'warn'
  | 'error'
  | 'fatal'

/**
 * The Logger class for creating and managing log messages.
 */
export class Logger {
  /** The format of the logs (json or pretty). */
  #_format: LogFormat

  /** The logging level. Logs will only be shown if their level is at or above this level. */
  #_level: LogLevel

  /** The ordered list of log levels. Logging is enabled for levels greater than or equal to the set level. */
  #logLevels: Array<LogLevel> = [
    'silly',
    'trace',
    'debug',
    'info',
    'warn',
    'error',
    'fatal',
  ]

  /** The color coding for each log level. */
  #logColors: Array<[string, string]> = [
    ['\x1b[35m', '\x1b[0m'], // silly - magenta
    ['\x1b[34m', '\x1b[0m'], // trace - blue
    ['\x1b[36m', '\x1b[0m'], // debug - cyan
    ['\x1b[32m', '\x1b[0m'], // info - green
    ['\x1b[33m', '\x1b[0m'], // warn - yellow
    ['\x1b[31m', '\x1b[0m'], // error - red
    ['\x1b[41m', '\x1b[0m'], // fatal - background red
  ]

  /**
   * Determines if the message should be logged based on the current log level.
   * @param level - The log level of the message.
   * @returns `true` if the message should be logged, otherwise `false`.
   */
  #shouldLog(level: LogLevel) {
    return this.#logLevels.indexOf(level) >= this.#logLevels.indexOf(this.level)
  }

  /**
   * Formats the log message as a JSON string.
   * @param level - The log level of the message.
   * @param args - The arguments to log.
   * @returns The JSON formatted log message.
   */
  #getJSONFormattedArgs(level: LogLevel, ...args: unknown[]) {
    return [JSON.stringify({ level, message: args })]
  }

  /**
   * Formats the log message in a human-readable format with colors.
   * @param level - The log level of the message.
   * @param args - The arguments to log.
   * @returns The pretty formatted log message.
   */
  #getPrettyFormatedArgs(level: LogLevel, ...args: unknown[]) {
    return [
      `${this.#getFormattedDate()} ${this.#getColorFormattedLevel(level)}`,
      ...args,
    ]
  }

  /**
   * Formats the log message according to the current format.
   * @param level - The log level of the message.
   * @param args - The arguments to log.
   * @returns The formatted log message.
   */
  #getFormattedArgs(level: LogLevel, ...args: unknown[]) {
    if (this.format === 'json') {
      return this.#getJSONFormattedArgs(level, ...args)
    }

    if (this.format === 'pretty') {
      return this.#getPrettyFormatedArgs(level, ...args)
    }

    return this.#getPrettyFormatedArgs(level, ...args)
  }

  /**
   * Gets the current timestamp formatted as a string.
   * @returns The formatted date string.
   */
  #getFormattedDate() {
    return new Date().toISOString()
  }

  /**
   * Gets the color formatted log level string.
   * @param level - The log level to format.
   * @returns The color formatted log level string.
   */
  #getColorFormattedLevel(level: LogLevel) {
    const index = this.#logLevels.indexOf(level)
    const logColors = this.#logColors[index]
    if (logColors === undefined) {
      throw new Error(`Log color not found for level: ${level}`)
    }
    const [start, end] = logColors
    return `${start}${level}${end}`
  }

  /**
   * Constructs a new Logger instance.
   * @param format - The log format (json or pretty). Default is 'pretty'.
   * @param level - The initial log level. Default is 'debug'.
   */
  constructor(format: LogFormat = 'pretty', level: LogLevel = 'debug') {
    this.#_format = format
    this.#_level = level
  }

  /**
   * Gets the current log format.
   * @returns The current log format.
   */
  get format() {
    return this.#_format
  }

  /**
   * Sets the log format.
   * @param format - The new log format.
   */
  set format(format: LogFormat) {
    this.#_format = format
  }

  /**
   * Gets the current log level.
   * @returns The current log level.
   */
  get level() {
    return this.#_level
  }

  /**
   * Sets the log level.
   * @param level - The new log level.
   */
  set level(level: LogLevel) {
    this.#_level = level
  }

  /**
   * Creates a logger function for the given log level.
   * @param level - The log level.
   * @returns The logger function.
   */
  #createLogger = (level: LogLevel, fn: 'log' | 'error' = 'log') => {
    return (...args: unknown[]) => {
      if (this.#shouldLog(level)) {
        console[fn](...this.#getFormattedArgs(level, ...args))
      }
    }
  }

  /**
   * Logs a message at the 'silly' level.
   * @param args - The arguments to log.
   */
  silly = this.#createLogger('silly')

  /**
   * Logs a message at the 'trace' level.
   * @param args - The arguments to log.
   */
  trace = this.#createLogger('trace')

  /**
   * Logs a message at the 'debug' level.
   * @param args - The arguments to log.
   */
  debug = this.#createLogger('debug')

  /**
   * Logs a message at the 'info' level.
   * @param args - The arguments to log.
   */
  info = this.#createLogger('info')

  /**
   * Logs a message at the 'warn' level.
   * @param args - The arguments to log.
   */
  warn = this.#createLogger('warn')

  /**
   * Logs a message at the 'error' level.
   * @param args - The arguments to log.
   */
  error = this.#createLogger('error', 'error')

  /**
   * Logs a message at the 'fatal' level.
   * @param args - The arguments to log.
   */
  fatal = this.#createLogger('fatal', 'error')
}

const logger = new Logger()

export default logger
