import * as O from 'fp-ts/Option'
import { describe, expect, test } from 'vitest'

import { convertTime } from './date.ts'

describe('Time Conversion with convertTime Function', () => {
  test('Convert milliseconds to seconds', () => {
    expect(convertTime(1000, 'ms', 's')).toStrictEqual(O.some(1))
    expect(convertTime(5000, 'ms', 's')).toStrictEqual(O.some(5))
  })

  test('Convert seconds to milliseconds', () => {
    expect(convertTime(1, 's', 'ms')).toStrictEqual(O.some(1000))
    expect(convertTime(10, 's', 'ms')).toStrictEqual(O.some(10000))
  })

  test('Convert minutes to hours', () => {
    expect(convertTime(60, 'm', 'h')).toStrictEqual(O.some(1))
    expect(convertTime(120, 'm', 'h')).toStrictEqual(O.some(2))
  })

  test('Convert hours to days', () => {
    expect(convertTime(24, 'h', 'd')).toStrictEqual(O.some(1))
    expect(convertTime(48, 'h', 'd')).toStrictEqual(O.some(2))
  })
})
