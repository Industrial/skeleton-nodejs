import { describe, expect, test } from 'bun:test'

import { ema } from './ema.ts'

describe('ema', () => {
  describe('When passed a length of 5 and an array of 1 to 10', () => {
    test('Should return the correct sequence', () => {
      const expected = [
        1, 1.3333333333333335, 1.888888888888889, 2.5925925925925926, 3.3950617283950617, 4.263374485596708,
        5.175582990397805, 6.117055326931871, 7.078036884621247, 8.052024589747498,
      ]
      const actual = ema(5, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
      expect(actual).toEqual(expected)
    })
  })
})
