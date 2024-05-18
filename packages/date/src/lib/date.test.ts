import { describe, expect, test } from 'bun:test'
import { Option } from 'effect'

import { convertTime } from './date.ts'

describe('Time Conversion with convertTime Function', () => {
  test('Convert milliseconds to seconds', () => {
    expect(convertTime(1000, 'ms', 's')).toStrictEqual(Option.some(1))
    expect(convertTime(5000, 'ms', 's')).toStrictEqual(Option.some(5))
  })

  test('Convert seconds to milliseconds', () => {
    expect(convertTime(1, 's', 'ms')).toStrictEqual(Option.some(1000))
    expect(convertTime(10, 's', 'ms')).toStrictEqual(Option.some(10000))
  })

  test('Convert minutes to hours', () => {
    expect(convertTime(60, 'm', 'h')).toStrictEqual(Option.some(1))
    expect(convertTime(120, 'm', 'h')).toStrictEqual(Option.some(2))
  })

  test('Convert hours to days', () => {
    expect(convertTime(24, 'h', 'd')).toStrictEqual(Option.some(1))
    expect(convertTime(48, 'h', 'd')).toStrictEqual(Option.some(2))
  })
})
