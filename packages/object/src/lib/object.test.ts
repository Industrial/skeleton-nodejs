import { bag, entriesValues, sortedEntries } from '@code9/object'
import { describe, expect, it } from 'bun:test'

describe('entriesValues', () => {
  describe('When the array is empty', () => {
    it('should return an empty array', () => {
      const result = entriesValues([])
      expect(result).toEqual([])
    })
  })

  describe('When the array has entries', () => {
    it('should return an array of values from the entries', () => {
      const result = entriesValues([
        ['a', 1],
        ['b', 2],
        ['c', 3],
      ])
      expect(result).toEqual([1, 2, 3])
    })
  })
})

describe('sortedEntries', () => {
  describe('When the object is empty', () => {
    it('should return an empty array', () => {
      const result = sortedEntries({})
      expect(result).toEqual([])
    })
  })

  describe('When the object has entries', () => {
    describe('When the keys are in alphabetical order', () => {
      it('should return entries sorted by key', () => {
        const result = sortedEntries({
          banana: 1,
          apple: 2,
          orange: 3,
        })
        expect(result).toEqual([
          ['apple', 2],
          ['banana', 1],
          ['orange', 3],
        ])
      })
    })

    describe('When the keys are not in alphabetical order', () => {
      it('should return entries sorted by key', () => {
        const result = sortedEntries({
          1: 'one',
          2: 'two',
          3: 'three',
        })
        expect(result).toEqual([
          ['1', 'one'],
          ['2', 'two'],
          ['3', 'three'],
        ])
      })
    })
  })
})

describe('bag', () => {
  describe('When the size is zero', () => {
    it('should return an empty object', () => {
      const result = bag(0, {
        key1: 'value1',
        key2: 'value2',
      })
      expect(result).toEqual({})
    })
  })

  describe('When the size is not zero', () => {
    describe('When the size is larger than the object size', () => {
      it('should return the whole object', () => {
        const result = bag(3, {
          key1: 'value1',
          key2: 'value2',
        })
        expect(result).toEqual({
          key1: 'value1',
          key2: 'value2',
        })
      })
    })

    describe('When the size is smaller than the object size', () => {
      it('should return a bag of the specified size', () => {
        const result = bag(2, {
          key1: 'value1',
          key2: 'value2',
          key3: 'value3',
        })
        expect(Object.keys(result).length).toBe(2)
      })
    })

    describe('When a new entry is added with a key that does not exist in the object', () => {
      it('should return the object with the new entry included and limited to the specified size', () => {
        const result = bag(
          3,
          {
            key1: 'value1',
            key2: 'value2',
          },
          'value3',
          'key3',
        )
        expect(result.key3).toBe('value3')
      })
    })

    describe('When a new entry is added with a key that exists in the object', () => {
      it('should return the object with the updated entry included and limited to the specified size', () => {
        const result = bag(
          3,
          {
            key1: 'value1',
            key2: 'value2',
            key3: 'value3',
          },
          'value3-updated',
          'key3',
        )
        expect(result.key3).toBe('value3-updated')
      })
    })
  })
})
