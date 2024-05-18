import { describe, expect, test } from 'bun:test'

import { bag, entriesValues, sortedEntries } from './object.ts'

describe('entriesValues function', () => {
  test('should return an array of values from entries', () => {
    const entries: Array<[string, number]> = [
      ['a', 1],
      ['b', 2],
      ['c', 3],
    ]
    const result = entriesValues(entries)
    expect(result).toEqual([1, 2, 3])
  })

  test('should return an empty array for empty entries', () => {
    const entries: Array<[string, number]> = []
    const result = entriesValues(entries)
    expect(result).toEqual([])
  })
})

describe('sortedEntries function', () => {
  test('should return an empty array for an empty object', () => {
    const result = sortedEntries({})
    expect(result).toEqual([])
  })

  test('should sort object entries alphabetically by keys', () => {
    const input = {
      banana: 1,
      apple: 2,
      orange: 3,
    }

    const result = sortedEntries(input)
    const expected = [
      ['apple', 2],
      ['banana', 1],
      ['orange', 3],
    ]

    expect(result).toEqual(expected)
  })

  test('should handle objects with non-string keys', () => {
    const input = {
      3: 'three',
      1: 'one',
      2: 'two',
    }

    const result = sortedEntries(input)
    const expected = [
      ['1', 'one'],
      ['2', 'two'],
      ['3', 'three'],
    ]

    expect(result).toEqual(expected)
  })
})

describe('bag function', () => {
  test('returns an empty object when size is 0', () => {
    const result = bag(0, { key1: 'value1', key2: 'value2' })
    expect(result).toEqual({})
  })

  test('returns the whole object when size is larger than object size', () => {
    const object = { key1: 'value1', key2: 'value2' }
    const result = bag(3, object)
    expect(result).toEqual(object)
  })

  test('returns a bag of specific size when object has more entries', () => {
    const object = { key1: 'value1', key2: 'value2', key3: 'value3' }
    const result = bag(2, object)
    expect(Object.keys(result).length).toBe(2)
  })

  test('adds a new entry to the bag if k and v are provided', () => {
    const object = { key1: 'value1', key2: 'value2' }
    const result = bag(3, object, 'value3', 'key3')
    expect(result.key3).toBe('value3')
  })
})
