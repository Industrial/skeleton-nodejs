import { describe, expect, test } from 'vitest'

import { add, between, fromMs, start, subtract, toMs } from './timeframe.ts'

describe('timeframeToMilliseconds', () => {
  test('converts 1m', () => {
    expect(toMs('1m')).toEqual(60 * 1000)
  })

  test('converts 3m', () => {
    expect(toMs('3m')).toEqual(3 * 60 * 1000)
  })

  test('converts 5m', () => {
    expect(toMs('5m')).toEqual(5 * 60 * 1000)
  })

  test('converts 15m', () => {
    expect(toMs('15m')).toEqual(15 * 60 * 1000)
  })

  test('converts 30m', () => {
    expect(toMs('30m')).toEqual(30 * 60 * 1000)
  })

  test('converts 1h', () => {
    expect(toMs('1h')).toEqual(1 * 60 * 60 * 1000)
  })

  test('converts 2h', () => {
    expect(toMs('2h')).toEqual(2 * 60 * 60 * 1000)
  })

  test('converts 4h', () => {
    expect(toMs('4h')).toEqual(4 * 60 * 60 * 1000)
  })

  test('converts 6h', () => {
    expect(toMs('6h')).toEqual(6 * 60 * 60 * 1000)
  })

  test('converts 8h', () => {
    expect(toMs('8h')).toEqual(8 * 60 * 60 * 1000)
  })

  test('converts 12h', () => {
    expect(toMs('12h')).toEqual(12 * 60 * 60 * 1000)
  })

  test('converts 1d', () => {
    expect(toMs('1d')).toEqual(24 * 60 * 60 * 1000)
  })
})

describe('millisecondsToTimeframe', () => {
  test('converts 60 * 1000', () => {
    expect(fromMs(60 * 1000)).toEqual('1m')
  })

  test('converts 3 * 60 * 1000', () => {
    expect(fromMs(3 * 60 * 1000)).toEqual('3m')
  })

  test('converts 5 * 60 * 1000', () => {
    expect(fromMs(5 * 60 * 1000)).toEqual('5m')
  })

  test('converts 15 * 60 * 1000', () => {
    expect(fromMs(15 * 60 * 1000)).toEqual('15m')
  })

  test('converts 30 * 60 * 1000', () => {
    expect(fromMs(30 * 60 * 1000)).toEqual('30m')
  })

  test('converts 1 * 60 * 60 * 1000', () => {
    expect(fromMs(1 * 60 * 60 * 1000)).toEqual('1h')
  })

  test('converts 2 * 60 * 60 * 1000', () => {
    expect(fromMs(2 * 60 * 60 * 1000)).toEqual('2h')
  })

  test('converts 4 * 60 * 60 * 1000', () => {
    expect(fromMs(4 * 60 * 60 * 1000)).toEqual('4h')
  })

  test('converts 6 * 60 * 60 * 1000', () => {
    expect(fromMs(6 * 60 * 60 * 1000)).toEqual('6h')
  })

  test('converts 8 * 60 * 60 * 1000', () => {
    expect(fromMs(8 * 60 * 60 * 1000)).toEqual('8h')
  })

  test('converts 12 * 60 * 60 * 1000', () => {
    expect(fromMs(12 * 60 * 60 * 1000)).toEqual('12h')
  })

  test('converts 24 * 60 * 60 * 1000', () => {
    expect(fromMs(24 * 60 * 60 * 1000)).toEqual('1d')
  })
})

describe('addTimeframe', () => {
  test('adds 1m', () => {
    expect(add('1m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:01:00.000Z'))
  })

  test('adds 3m', () => {
    expect(add('3m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:03:00.000Z'))
  })

  test('adds 5m', () => {
    expect(add('5m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:05:00.000Z'))
  })

  test('adds 15m', () => {
    expect(add('15m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:15:00.000Z'))
  })

  test('adds 30m', () => {
    expect(add('30m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:30:00.000Z'))
  })

  test('adds 1h', () => {
    expect(add('1h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T01:00:00.000Z'))
  })

  test('adds 2h', () => {
    expect(add('2h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T02:00:00.000Z'))
  })

  test('adds 4h', () => {
    expect(add('4h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T04:00:00.000Z'))
  })

  test('adds 6h', () => {
    expect(add('6h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T06:00:00.000Z'))
  })

  test('adds 8h', () => {
    expect(add('8h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T08:00:00.000Z'))
  })

  test('adds 12h', () => {
    expect(add('12h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T12:00:00.000Z'))
  })

  test('adds 1d', () => {
    expect(add('1d', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-02T00:00:00.000Z'))
  })
})

describe('subtractTimeframe', () => {
  test('subtracts 1m', () => {
    expect(subtract('1m', new Date('2021-01-01T00:01:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 3m', () => {
    expect(subtract('3m', new Date('2021-01-01T00:03:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 5m', () => {
    expect(subtract('5m', new Date('2021-01-01T00:05:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 15m', () => {
    expect(subtract('15m', new Date('2021-01-01T00:15:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 30m', () => {
    expect(subtract('30m', new Date('2021-01-01T00:30:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 1h', () => {
    expect(subtract('1h', new Date('2021-01-01T01:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 2h', () => {
    expect(subtract('2h', new Date('2021-01-01T02:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 4h', () => {
    expect(subtract('4h', new Date('2021-01-01T04:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 6h', () => {
    expect(subtract('6h', new Date('2021-01-01T06:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 8h', () => {
    expect(subtract('8h', new Date('2021-01-01T08:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 12h', () => {
    expect(subtract('12h', new Date('2021-01-01T12:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('subtracts 1d', () => {
    expect(subtract('1d', new Date('2021-01-02T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })
})

describe('startOfTimeframe', () => {
  test('start of 1m', () => {
    expect(start('1m', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
    expect(start('1m', new Date('2021-01-01T00:00:59.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('start of 3m', () => {
    expect(start('3m', new Date('2021-01-01T00:03:00.000Z'))).toEqual(new Date('2021-01-01T00:03:00.000Z'))
    expect(start('3m', new Date('2021-01-01T00:05:59.000Z'))).toEqual(new Date('2021-01-01T00:03:00.000Z'))
  })

  test('start of 5m', () => {
    expect(start('5m', new Date('2021-01-01T00:05:00.000Z'))).toEqual(new Date('2021-01-01T00:05:00.000Z'))
    expect(start('5m', new Date('2021-01-01T00:09:59.000Z'))).toEqual(new Date('2021-01-01T00:05:00.000Z'))
  })

  test('start of 15m', () => {
    expect(start('15m', new Date('2021-01-01T00:15:00.000Z'))).toEqual(new Date('2021-01-01T00:15:00.000Z'))
    expect(start('15m', new Date('2021-01-01T00:29:59.000Z'))).toEqual(new Date('2021-01-01T00:15:00.000Z'))
  })

  test('start of 30m', () => {
    expect(start('30m', new Date('2021-01-01T00:30:00.000Z'))).toEqual(new Date('2021-01-01T00:30:00.000Z'))
    expect(start('30m', new Date('2021-01-01T00:59:59.000Z'))).toEqual(new Date('2021-01-01T00:30:00.000Z'))
  })

  test('start of 1h', () => {
    expect(start('1h', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
    expect(start('1h', new Date('2021-01-01T00:59:59.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })

  test('start of 2h', () => {
    expect(start('2h', new Date('2021-01-01T02:00:00.000Z'))).toEqual(new Date('2021-01-01T02:00:00.000Z'))
    expect(start('2h', new Date('2021-01-01T03:59:59.000Z'))).toEqual(new Date('2021-01-01T02:00:00.000Z'))
  })

  test('start of 4h', () => {
    expect(start('4h', new Date('2021-01-01T04:00:00.000Z'))).toEqual(new Date('2021-01-01T04:00:00.000Z'))
    expect(start('4h', new Date('2021-01-01T07:59:59.000Z'))).toEqual(new Date('2021-01-01T04:00:00.000Z'))
  })

  test('start of 6h', () => {
    expect(start('6h', new Date('2021-01-01T06:00:00.000Z'))).toEqual(new Date('2021-01-01T06:00:00.000Z'))
    expect(start('6h', new Date('2021-01-01T07:59:59.000Z'))).toEqual(new Date('2021-01-01T06:00:00.000Z'))
  })

  test('start of 8h', () => {
    expect(start('8h', new Date('2021-01-01T08:00:00.000Z'))).toEqual(new Date('2021-01-01T08:00:00.000Z'))
    expect(start('8h', new Date('2021-01-01T11:59:59.000Z'))).toEqual(new Date('2021-01-01T08:00:00.000Z'))
  })

  test('start of 12h', () => {
    expect(start('12h', new Date('2021-01-01T12:00:00.000Z'))).toEqual(new Date('2021-01-01T12:00:00.000Z'))
    expect(start('12h', new Date('2021-01-01T13:59:59.000Z'))).toEqual(new Date('2021-01-01T12:00:00.000Z'))
  })

  test('start of 1d', () => {
    expect(start('1d', new Date('2021-01-01T00:00:00.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
    expect(start('1d', new Date('2021-01-01T23:59:59.000Z'))).toEqual(new Date('2021-01-01T00:00:00.000Z'))
  })
})

describe('between', () => {
  test('between 1m', () => {
    expect(between('1m', new Date('2021-01-01T00:00:00.000Z'), new Date('2021-01-01T00:00:59.000Z'))).toStrictEqual([])
    expect(between('1m', new Date('2021-01-01T00:00:00.000Z'), new Date('2021-01-01T00:01:00.000Z'))).toStrictEqual([
      new Date('2021-01-01T00:00:00.000Z'),
    ])
    expect(between('1m', new Date('2021-01-01T00:00:00.000Z'), new Date('2021-01-01T00:10:00.000Z'))).toStrictEqual([
      new Date('2021-01-01T00:00:00.000Z'),
      new Date('2021-01-01T00:01:00.000Z'),
      new Date('2021-01-01T00:02:00.000Z'),
      new Date('2021-01-01T00:03:00.000Z'),
      new Date('2021-01-01T00:04:00.000Z'),
      new Date('2021-01-01T00:05:00.000Z'),
      new Date('2021-01-01T00:06:00.000Z'),
      new Date('2021-01-01T00:07:00.000Z'),
      new Date('2021-01-01T00:08:00.000Z'),
      new Date('2021-01-01T00:09:00.000Z'),
    ])
  })
})
