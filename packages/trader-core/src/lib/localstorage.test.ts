import { afterEach, describe, expect, test, vi } from 'vitest'

import { createKey, del, get, set } from './localstorage.ts'

describe('createKey function', () => {
  test('should return a string', () => {
    // Arrange
    const expected = 'browser_trader_test'

    // Act
    const actual = createKey('test')

    // Assert
    expect(actual).toBe(expected)
  })
})

describe('get function', () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  test('should return a string', () => {
    // Arrange
    const expected = undefined

    // Act
    const actual = get('test')

    // Assert
    expect(actual).toEqual(expected)
  })
})

describe('set function', () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  test('should return the value through get', () => {
    // Arrange
    const expected = 'derp'

    // Act
    set('herp', 'derp')
    const actual = get('herp')

    // Assert
    expect(actual).toEqual(expected)
  })
})

describe('del function', () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  test('should delete the value in the localStorage', () => {
    // Arrange
    const expected = undefined

    // Act
    set('herp', 'derp')
    del('herp')
    const actual = get('herp')

    // Assert
    expect(actual).toEqual(expected)
  })
})
