import {
  type Mock,
  afterEach,
  beforeEach,
  describe,
  expect,
  it,
  spyOn,
} from 'bun:test'
import * as ccxt from 'ccxt'
import type { binance } from 'ccxt'
import { Effect, Either } from 'effect'
import type { ParseError } from 'effect/ParseResult'
import * as Candlestick from '../domain/market-data/Candlestick'
import {
  type Candlestick as CandlestickType,
  InvalidPriceRelationshipError,
  InvalidTimestampError,
  InvalidVolumeError,
} from '../domain/market-data/Candlestick'
import type { ExchangeId } from '../domain/market-data/ExchangeId'
import type { Pair } from '../domain/market-data/Pair'
import * as Timeframe from '../domain/market-data/Timeframe'
import { DateRangeError, UnsupportedExchangeError } from './CryptoDataService'
import * as CryptoDataServiceLive from './CryptoDataServiceLive'

describe('When using CryptoDataServiceLive', () => {
  let fromCCXTSpy: Mock<typeof Candlestick.fromCCXT>
  let toMillisecondsSpy: Mock<typeof Timeframe.toMilliseconds>
  let getExchangeClassSpy: Mock<typeof CryptoDataServiceLive.getExchangeClass>

  beforeEach(() => {
    fromCCXTSpy = spyOn(Candlestick, 'fromCCXT')
    toMillisecondsSpy = spyOn(Timeframe, 'toMilliseconds')
    getExchangeClassSpy = spyOn(CryptoDataServiceLive, 'getExchangeClass')

    // Default implementation returns the real exchange class
    // getExchangeClassSpy.mockImplementation(originalGetExchangeClass)
    fromCCXTSpy.mockImplementation((ohlcv: ccxt.OHLCV) => {
      const [timestamp, open, high, low, close, volume] = ohlcv

      if (timestamp === 0) {
        return Effect.fail(
          new InvalidTimestampError({
            message: 'Invalid timestamp',
          }),
        )
      }
      if (volume && volume < 0) {
        return Effect.fail(
          new InvalidVolumeError({
            message: 'Volume must be non-negative',
          }),
        )
      }
      if (high && open && high < open) {
        // High < Open
        return Effect.fail(
          new InvalidPriceRelationshipError({
            message: 'Invalid price relationship',
          }),
        )
      }
      return Effect.succeed({
        timestamp,
        open,
        high,
        low,
        close,
        volume,
      } as CandlestickType)
    })

    toMillisecondsSpy.mockImplementation((timeframe: Timeframe.Timeframe) => {
      if (timeframe === ('invalid' as Timeframe.Timeframe)) {
        return Effect.fail({
          _tag: 'ParseError',
          message: 'Invalid timeframe',
        } as ParseError)
      }
      const timeframeMap: Record<string, number> = {
        '1m': 60 * 1000,
        '1h': 60 * 60 * 1000,
        '1d': 24 * 60 * 60 * 1000,
      }
      return Effect.succeed(timeframeMap[timeframe] || 60000)
    })

    getExchangeClassSpy.mockImplementation((exchangeId: ExchangeId) => {
      return Effect.succeed(
        ccxt[
          exchangeId as keyof typeof ccxt
        ] as unknown as new () => ccxt.Exchange,
      )
    })
  })

  afterEach(() => {
    fromCCXTSpy.mockRestore()
    toMillisecondsSpy.mockRestore()
    getExchangeClassSpy.mockRestore()
  })

  describe('When calling getExchangeClass method', () => {
    describe('When providing a valid exchange ID', () => {
      it('should return the exchange class constructor', async () => {
        const exchangeId: ExchangeId = 'binance'
        const result = CryptoDataServiceLive.getExchangeClass(exchangeId)

        return await Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isRight(either)).toBe(true)

          if (Either.isRight(either)) {
            const ExchangeClass = either.right
            expect(ExchangeClass).toBeDefined()
            expect(typeof ExchangeClass).toBe('function')

            // Create an instance to verify it's a valid constructor
            const exchange = new ExchangeClass()
            expect(exchange).toBeDefined()
            expect(typeof exchange.fetchOHLCV).toBe('function')
          }
        })
      })
    })
  })

  describe('When calling getOHLCV method', () => {
    describe('When providing a valid exchange ID', () => {
      describe('When providing a valid pair', () => {
        describe('When providing a valid timeframe', () => {
          // biome-ignore lint/complexity/noExcessiveNestedTestSuites: <explanation>
          describe('When providing valid start and end dates', () => {
            describe('When the exchange API returns valid data', () => {
              it('should return an array of candlesticks', async () => {
                /* Test successful retrieval of candlesticks */
                // Mock getExchangeClass to return a MockValidDataExchange
                class MockValidDataExchange {
                  async fetchOHLCV() {
                    return [
                      [1625097600000, 35000, 36000, 34500, 35500, 10.5],
                      [1625184000000, 35500, 36500, 35000, 36000, 15.2],
                    ]
                  }
                }
                getExchangeClassSpy.mockImplementationOnce(() => {
                  return Effect.succeed(
                    MockValidDataExchange as unknown as new () => ccxt.Exchange,
                  )
                })

                const exchangeId: ExchangeId = 'binance'
                const pair: Pair = 'BTC/USDT'
                const timeframe: Timeframe.Timeframe = '1h'
                const start = new Date('2023-01-01T00:00:00Z')
                const end = new Date('2023-01-02T00:00:00Z')

                const result = CryptoDataServiceLive.getOHLCV(
                  exchangeId,
                  pair,
                  timeframe,
                  start,
                  end,
                )

                await Effect.runPromise(Effect.either(result)).then(
                  (either) => {
                    expect(Either.isRight(either)).toBe(true)

                    if (Either.isRight(either)) {
                      const candlesticks = either.right
                      expect(candlesticks).toHaveLength(2)

                      expect(candlesticks[0]).toEqual({
                        timestamp: 1625097600000,
                        open: 35000,
                        high: 36000,
                        low: 34500,
                        close: 35500,
                        volume: 10.5,
                      })

                      expect(candlesticks[1]).toEqual({
                        timestamp: 1625184000000,
                        open: 35500,
                        high: 36500,
                        low: 35000,
                        close: 36000,
                        volume: 15.2,
                      })
                    }
                  },
                )
              })
            })

            describe('When the exchange API returns empty data', () => {
              it('should return an empty array', async () => {
                /* Test empty data response */
                // Create a mock exchange class that returns empty data
                class MockEmptyExchange {
                  async fetchOHLCV() {
                    return []
                  }
                }
                getExchangeClassSpy.mockImplementationOnce(() =>
                  Effect.succeed(
                    MockEmptyExchange as unknown as new () => ccxt.Exchange,
                  ),
                )

                const exchangeId: ExchangeId = 'binance'
                const pair: Pair = 'BTC/USDT'
                const timeframe: Timeframe.Timeframe = '1h'
                const start = new Date('2023-01-01T00:00:00Z')
                const end = new Date('2023-01-02T00:00:00Z')

                const result = CryptoDataServiceLive.getOHLCV(
                  exchangeId,
                  pair,
                  timeframe,
                  start,
                  end,
                )

                await Effect.runPromise(Effect.either(result)).then(
                  (either) => {
                    expect(Either.isRight(either)).toBe(true)

                    if (Either.isRight(either)) {
                      const candlesticks = either.right
                      expect(candlesticks).toHaveLength(0)
                    }
                  },
                )
              })
            })

            describe('When the exchange API returns data with invalid price relationships', () => {
              it('should propagate the InvalidPriceRelationshipError', () => {
                /* Test invalid price relationship error handling */
                // Create a mock exchange class that returns data with invalid price relationships
                class MockInvalidPriceExchange {
                  async fetchOHLCV() {
                    return [
                      [1625097600000, 36000, 35000, 34500, 35500, 10.5], // High (35000) < Open (36000) - invalid
                    ]
                  }
                }
                getExchangeClassSpy.mockImplementationOnce(() => {
                  return Effect.succeed(
                    MockInvalidPriceExchange as unknown as new () => ccxt.Exchange,
                  )
                })

                const exchangeId: ExchangeId = 'binance'
                const pair: Pair = 'BTC/USDT'
                const timeframe: Timeframe.Timeframe = '1h'
                const start = new Date('2023-01-01T00:00:00Z')
                const end = new Date('2023-01-02T00:00:00Z')

                const result = CryptoDataServiceLive.getOHLCV(
                  exchangeId,
                  pair,
                  timeframe,
                  start,
                  end,
                )

                return Effect.runPromise(Effect.either(result)).then(
                  (either) => {
                    expect(Either.isLeft(either)).toBe(true)

                    if (Either.isLeft(either)) {
                      const error = either.left
                      expect(error).toBeDefined()
                      expect(error).toBeInstanceOf(
                        InvalidPriceRelationshipError,
                      )
                      expect(error.toString()).toContain(
                        'Invalid price relationship',
                      )
                    }
                  },
                )
              })
            })

            describe('When the exchange API returns data with negative volume', () => {
              it('should propagate the InvalidVolumeError', () => {
                /* Test negative volume error handling */
                // Create a mock exchange class that returns data with negative volume
                class MockNegativeVolumeExchange {
                  async fetchOHLCV() {
                    return [
                      [1625097600000, 35000, 36000, 34500, 35500, -10.5], // Negative volume - invalid
                    ]
                  }
                }
                getExchangeClassSpy.mockImplementationOnce(() =>
                  Effect.succeed(
                    MockNegativeVolumeExchange as unknown as new () => ccxt.Exchange,
                  ),
                )

                const exchangeId: ExchangeId = 'binance'
                const pair: Pair = 'BTC/USDT'
                const timeframe: Timeframe.Timeframe = '1h'
                const start = new Date('2023-01-01T00:00:00Z')
                const end = new Date('2023-01-02T00:00:00Z')

                const result = CryptoDataServiceLive.getOHLCV(
                  exchangeId,
                  pair,
                  timeframe,
                  start,
                  end,
                )

                return Effect.runPromise(Effect.either(result)).then(
                  (either) => {
                    expect(Either.isLeft(either)).toBe(true)

                    if (Either.isLeft(either)) {
                      const error = either.left
                      expect(error).toBeDefined()
                      expect(error).toBeInstanceOf(InvalidVolumeError)
                      expect(error.toString()).toContain(
                        'Volume must be non-negative',
                      )
                    }
                  },
                )
              })
            })

            describe('When the exchange API returns data with invalid timestamps', () => {
              it('should propagate the InvalidTimestampError', () => {
                /* Test invalid timestamp error handling */
                // Create a mock exchange class that returns data with invalid timestamps
                class MockInvalidTimestampExchange {
                  async fetchOHLCV() {
                    return [
                      [0, 35000, 36000, 34500, 35500, 10.5], // Zero timestamp - invalid
                    ]
                  }
                }
                getExchangeClassSpy.mockImplementation(() =>
                  Effect.succeed(
                    MockInvalidTimestampExchange as unknown as new () => ccxt.Exchange,
                  ),
                )

                const exchangeId: ExchangeId = 'binance'
                const pair: Pair = 'BTC/USDT'
                const timeframe: Timeframe.Timeframe = '1h'
                const start = new Date('2023-01-01T00:00:00Z')
                const end = new Date('2023-01-02T00:00:00Z')

                const result = CryptoDataServiceLive.getOHLCV(
                  exchangeId,
                  pair,
                  timeframe,
                  start,
                  end,
                )

                return Effect.runPromise(Effect.either(result)).then(
                  (either) => {
                    expect(Either.isLeft(either)).toBe(true)

                    if (Either.isLeft(either)) {
                      const error = either.left
                      expect(error).toBeDefined()
                      expect(error).toBeInstanceOf(InvalidTimestampError)
                      expect(error.toString()).toContain('Invalid timestamp')
                    }
                  },
                )
              })
            })
          })

          // biome-ignore lint/complexity/noExcessiveNestedTestSuites: <explanation>
          describe('When the start date is after the end date', () => {
            it('should throw a DateRangeError', async () => {
              /* Test behavior with invalid date range */
              const exchangeId: ExchangeId = 'binance'
              const pair: Pair = 'BTC/USDT'
              const timeframe: Timeframe.Timeframe = '1h'
              // Start date is AFTER end date
              const start = new Date('2023-01-02T00:00:00Z')
              const end = new Date('2023-01-01T00:00:00Z')

              const result = CryptoDataServiceLive.getOHLCV(
                exchangeId,
                pair,
                timeframe,
                start,
                end,
              )

              return await Effect.runPromise(Effect.either(result)).then(
                (either) => {
                  expect(Either.isLeft(either)).toBe(true)

                  if (Either.isLeft(either)) {
                    const error = either.left
                    expect(error).toBeDefined()
                    expect(error).toBeInstanceOf(DateRangeError)
                    expect(error.toString()).toContain('DateRangeError')

                    // Verify the error contains the correct start and end dates
                    if (error instanceof DateRangeError) {
                      expect(error.start).toEqual(start)
                      expect(error.end).toEqual(end)
                    }
                  }
                },
              )
            })
          })

          // biome-ignore lint/complexity/noExcessiveNestedTestSuites: <explanation>
          describe('When the calculated limit is very large', () => {
            it('should use the calculated limit for the API call', async () => {
              /* Test behavior with large time range */
              // Create a spy for the exchange's fetchOHLCV method
              class MockExchange {
                async fetchOHLCV(
                  _pair: string,
                  _timeframe: string,
                  _since: number,
                  limit: number,
                ) {
                  // Verify limit is large (> 1000)
                  expect(limit).toBeGreaterThan(1000)
                  return []
                }
              }
              getExchangeClassSpy.mockImplementation(() =>
                Effect.succeed(
                  MockExchange as unknown as new () => ccxt.Exchange,
                ),
              )

              const exchangeId: ExchangeId = 'binance'
              const pair: Pair = 'BTC/USDT'
              const timeframe: Timeframe.Timeframe = '1h'
              // Use a very large time range (1 year)
              const start = new Date('2022-01-01T00:00:00Z')
              const end = new Date('2023-01-01T00:00:00Z')

              const result = CryptoDataServiceLive.getOHLCV(
                exchangeId,
                pair,
                timeframe,
                start,
                end,
              )

              await Effect.runPromise(Effect.either(result)).then((either) => {
                expect(Either.isRight(either)).toBe(true)
              })
            })
          })
        })
      })

      describe('When providing an empty or invalid pair', () => {
        it('should attempt to fetch data with the provided pair', async () => {
          /* Test behavior with invalid pair */
          // Mock getExchangeClass to return a MockExchange
          class MockExchange {
            async fetchOHLCV(
              pair: string,
              _timeframe: string,
              _since: number,
              _limit: number,
            ) {
              // Verify pair is empty
              expect(pair).toBe('')
              return []
            }
          }
          getExchangeClassSpy.mockImplementation(() =>
            Effect.succeed(MockExchange as unknown as new () => ccxt.Exchange),
          )

          const exchangeId: ExchangeId = 'binance'
          const invalidPair = '' as Pair
          const timeframe: Timeframe.Timeframe = '1h'
          const start = new Date('2023-01-01T00:00:00Z')
          const end = new Date('2023-01-02T00:00:00Z')

          const result = CryptoDataServiceLive.getOHLCV(
            exchangeId,
            invalidPair,
            timeframe,
            start,
            end,
          )

          return await Effect.runPromise(Effect.either(result)).then(
            (either) => {
              expect(Either.isRight(either)).toBe(true)
            },
          )
        })
      })
    })

    describe('When the toMilliseconds function fails', () => {
      it('should propagate the error', () => {
        /* Test timeframe conversion error handling */
        // We'll use the 'invalid' timeframe which is already set up to fail in the beforeEach
        const exchangeId: ExchangeId = 'binance'
        const pair: Pair = 'BTC/USDT'
        const timeframe = 'invalid' as Timeframe.Timeframe
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-02T00:00:00Z')

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isLeft(either)).toBe(true)

          if (Either.isLeft(either)) {
            const error = either.left
            expect(error).toBeDefined()
            expect(error).toHaveProperty('_tag', 'ParseError')
            expect(error).toHaveProperty('message', 'Invalid timeframe')
          }
        })
      })
    })

    describe('When the fromCCXT function fails for any item', () => {
      it('should fail with the first encountered error', () => {
        /* Test error handling when any item fails validation */
        // Create a mock exchange class that returns a mix of valid and invalid data
        class MockMixedDataExchange {
          async fetchOHLCV() {
            return [
              [1625097600000, 35000, 36000, 34500, 35500, 10.5], // Valid
              [0, 35000, 36000, 34500, 35500, 10.5], // Invalid timestamp - this should cause the failure
              [1625184000000, 36000, 35000, 34500, 35500, 10.5], // Invalid price relationship
              [1625270400000, 35000, 36000, 34500, 35500, -10.5], // Invalid volume
              [1625356800000, 35500, 36500, 35000, 36000, 15.2], // Valid
            ]
          }
        }
        getExchangeClassSpy.mockImplementation(() =>
          Effect.succeed(
            MockMixedDataExchange as unknown as new () => ccxt.Exchange,
          ),
        )

        const exchangeId = 'mixed-data-exchange' as ExchangeId
        const pair: Pair = 'BTC/USDT'
        const timeframe: Timeframe.Timeframe = '1h'
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-02T00:00:00Z')

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isLeft(either)).toBe(true)

          if (Either.isLeft(either)) {
            const error = either.left
            expect(error).toBeDefined()
            expect(error).toBeInstanceOf(InvalidTimestampError)
            expect(error.toString()).toContain('Invalid timestamp')
          }
        })
      })
    })
  })

  describe('When examining the module exports', () => {
    it('should export the getExchangeClass and getOHLCV functions', () => {
      /* Test module exports */
      expect(CryptoDataServiceLive).toBeDefined()
      expect(CryptoDataServiceLive.getExchangeClass).toBeDefined()
      expect(typeof CryptoDataServiceLive.getExchangeClass).toBe('function')
      expect(CryptoDataServiceLive.getOHLCV).toBeDefined()
      expect(typeof CryptoDataServiceLive.getOHLCV).toBe('function')
    })
  })

  describe('When calculating parameters for the CCXT API call', () => {
    describe('When the time difference is small', () => {
      it('should calculate an appropriate limit value', () => {
        /* Test limit calculation for small time ranges */
        // Create a mock exchange class with static properties to track calls
        class MockExchange {
          static lastPair: string | undefined
          static lastTimeframe: string | undefined
          static lastSince: number | undefined
          static lastLimit: number | undefined

          async fetchOHLCV(
            pair: string,
            timeframe: string,
            since?: number,
            limit?: number,
          ) {
            // Store the parameters for later verification
            MockExchange.lastPair = pair
            MockExchange.lastTimeframe = timeframe
            MockExchange.lastSince = since
            MockExchange.lastLimit = limit
            return []
          }
        }

        // Reset static properties
        MockExchange.lastPair = undefined
        MockExchange.lastTimeframe = undefined
        MockExchange.lastSince = undefined
        MockExchange.lastLimit = undefined

        getExchangeClassSpy.mockImplementation(() =>
          Effect.succeed(MockExchange as unknown as new () => ccxt.Exchange),
        )

        const exchangeId: ExchangeId = 'binance'
        const pair: Pair = 'BTC/USDT'
        const timeframe: Timeframe.Timeframe = '1h'
        // Use a small time range (2 hours)
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-01T02:00:00Z')

        // Calculate the expected limit
        const timeDiff = end.getTime() - start.getTime() // 2 hours = 7,200,000 ms
        const timeframeMs = 60 * 60 * 1000 // 1h in ms = 3,600,000 ms
        const expectedLimit = Math.ceil(timeDiff / timeframeMs) // Should be 2

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isRight(either)).toBe(true)
          // Verify the parameters passed to fetchOHLCV
          expect(MockExchange.lastPair).toBe(pair)
          expect(MockExchange.lastTimeframe).toBe(timeframe)
          expect(MockExchange.lastSince).toBe(start.getTime())
          expect(MockExchange.lastLimit).toBe(expectedLimit)
        })
      })
    })

    describe('When the time difference is large', () => {
      it('should calculate a large limit value', () => {
        /* Test limit calculation for large time ranges */
        // Create a mock exchange class with static properties to track calls
        class MockExchange {
          static lastPair: string | undefined
          static lastTimeframe: string | undefined
          static lastSince: number | undefined
          static lastLimit: number | undefined

          async fetchOHLCV(
            pair: string,
            timeframe: string,
            since?: number,
            limit?: number,
          ) {
            // Store the parameters for later verification
            MockExchange.lastPair = pair
            MockExchange.lastTimeframe = timeframe
            MockExchange.lastSince = since
            MockExchange.lastLimit = limit
            return []
          }
        }

        // Reset static properties
        MockExchange.lastPair = undefined
        MockExchange.lastTimeframe = undefined
        MockExchange.lastSince = undefined
        MockExchange.lastLimit = undefined

        getExchangeClassSpy.mockImplementation(() =>
          Effect.succeed(MockExchange as unknown as new () => ccxt.Exchange),
        )

        const exchangeId: ExchangeId = 'binance'
        const pair: Pair = 'BTC/USDT'
        const timeframe: Timeframe.Timeframe = '1h'
        // Use a large time range (1 month)
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-02-01T00:00:00Z')

        // Calculate the expected limit
        const timeDiff = end.getTime() - start.getTime() // ~31 days = ~2,678,400,000 ms
        const timeframeMs = 60 * 60 * 1000 // 1h in ms = 3,600,000 ms
        const expectedLimit = Math.ceil(timeDiff / timeframeMs) // ~744 (hours in a month)

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isRight(either)).toBe(true)
          // Verify the parameters passed to fetchOHLCV
          expect(MockExchange.lastPair).toBe(pair)
          expect(MockExchange.lastTimeframe).toBe(timeframe)
          expect(MockExchange.lastSince).toBe(start.getTime())
          expect(MockExchange.lastLimit).toBe(expectedLimit)
        })
      })
    })

    describe('When the start date equals the end date', () => {
      it('should throw a DateRangeError', async () => {
        /* Test behavior with equal dates */
        const exchangeId: ExchangeId = 'binance'
        const pair: Pair = 'BTC/USDT'
        const timeframe: Timeframe.Timeframe = '1h'
        // Same date for start and end
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-01T00:00:00Z')

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        return await Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isLeft(either)).toBe(true)

          if (Either.isLeft(either)) {
            const error = either.left
            expect(error).toBeDefined()
            expect(error).toBeInstanceOf(DateRangeError)
            expect(error.toString()).toContain('DateRangeError')

            // Verify the error contains the correct start and end dates
            if (error instanceof DateRangeError) {
              expect(error.start).toEqual(start)
              expect(error.end).toEqual(end)
            }
          }
        })
      })
    })
  })

  describe('When handling the Effect chain', () => {
    describe('When any step in the chain fails', () => {
      it('should propagate the error to the caller', () => {
        /* Test error propagation in Effect chain */
        // We'll use the toMilliseconds function failing as an example of a step in the chain failing
        // This is already tested in the "When the toMilliseconds function fails" test
        // But we'll use it here to demonstrate error propagation in the Effect chain

        const exchangeId: ExchangeId = 'binance'
        const pair: Pair = 'BTC/USDT'
        const timeframe = 'invalid' as Timeframe.Timeframe
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-02T00:00:00Z')

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        // The Effect chain should propagate the error from toMilliseconds
        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isLeft(either)).toBe(true)

          if (Either.isLeft(either)) {
            const error = either.left
            expect(error).toBeDefined()
            expect(error).toHaveProperty('_tag', 'ParseError')
            expect(error).toHaveProperty('message', 'Invalid timeframe')
          }
        })
      })
    })

    describe('When all steps succeed', () => {
      it('should return the final result', () => {
        /* Test successful Effect chain execution */
        // This is already tested in the "When the exchange API returns valid data" test
        // But we'll use it here to demonstrate successful Effect chain execution

        // Mock getExchangeClass to return a MockValidDataExchange
        class MockValidDataExchange {
          async fetchOHLCV() {
            return [
              [1625097600000, 35000, 36000, 34500, 35500, 10.5],
              [1625184000000, 35500, 36500, 35000, 36000, 15.2],
            ]
          }
        }
        getExchangeClassSpy.mockImplementation(() =>
          Effect.succeed(
            MockValidDataExchange as unknown as new () => ccxt.Exchange,
          ),
        )

        const exchangeId = 'valid-data-exchange' as ExchangeId
        const pair: Pair = 'BTC/USDT'
        const timeframe: Timeframe.Timeframe = '1h'
        const start = new Date('2023-01-01T00:00:00Z')
        const end = new Date('2023-01-02T00:00:00Z')

        const result = CryptoDataServiceLive.getOHLCV(
          exchangeId,
          pair,
          timeframe,
          start,
          end,
        )

        // The Effect chain should succeed and return the final result
        return Effect.runPromise(Effect.either(result)).then((either) => {
          expect(Either.isRight(either)).toBe(true)

          if (Either.isRight(either)) {
            const candlesticks = either.right
            expect(candlesticks).toHaveLength(2)

            expect(candlesticks[0]).toEqual({
              timestamp: 1625097600000,
              open: 35000,
              high: 36000,
              low: 34500,
              close: 35500,
              volume: 10.5,
            })

            expect(candlesticks[1]).toEqual({
              timestamp: 1625184000000,
              open: 35500,
              high: 36500,
              low: 35000,
              close: 36000,
              volume: 15.2,
            })
          }
        })
      })
    })
  })
})
