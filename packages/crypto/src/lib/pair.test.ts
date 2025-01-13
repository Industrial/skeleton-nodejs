import { describe, expect, it } from 'bun:test'
import { Either as E } from 'effect'
import {
  type Base,
  type Pair,
  type Quote,
  createPair,
  getBase,
  getQuote,
  isCorrectBaseE,
  isCorrectPairFormatE,
  isCorrectQuoteE,
} from '../lib/pair.ts'

describe('Pair module', () => {
  describe('isCorrectBaseE', () => {
    describe('When base is an empty string', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('' as Base)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains lower case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('abc' as Base)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains characters other then upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('ABC@!#$' as Base)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('ABC' as Base)
        expect(E.isRight(actual)).toStrictEqual(true)
      })
    })
  })

  describe('isCorrectQuoteE', () => {
    describe('When base is an empty string', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains lower case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('abc' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains characters other then upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('ABC@!#$' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base contains upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('ABC' as Quote)
        expect(E.isRight(actual)).toStrictEqual(true)
      })
    })
  })

  describe('isCorrectPairFormat', () => {
    describe('When format has a length lower then 2', () => {
      it('should return an error', () => {
        const actual = isCorrectPairFormatE(['BASE'])
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When format has a length higher or equal then 2', () => {
      it('should return the array', () => {
        const actual = isCorrectPairFormatE(['BASE', 'QUOTE'])
        if (E.isLeft(actual)) {
          throw actual.left
        }
        expect(E.isRight(actual)).toBe(true)
        expect(actual.right).toStrictEqual(['BASE', 'QUOTE'])
      })
    })
  })

  describe('createPair', () => {
    describe('When base is empty', () => {
      it('should return an error', () => {
        const actual = createPair('' as Base, 'QUOTE' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When quote is empty', () => {
      it('should return an error', () => {
        const actual = createPair('BASE' as Base, '' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base is not uppercase', () => {
      it('should return an error', () => {
        const actual = createPair('base' as Base, 'QUOTE' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When quote is not uppercase', () => {
      it('should return an error', () => {
        const actual = createPair('BASE' as Base, 'quote' as Quote)
        expect(E.isLeft(actual)).toStrictEqual(true)
      })
    })

    describe('When base and quote are uppercase', () => {
      it('should create a pair', () => {
        const actual = createPair('BASE' as Base, 'QUOTE' as Quote)
        if (E.isLeft(actual)) {
          throw actual.left
        }
        expect(actual.right).toStrictEqual('BASE/QUOTE' as Pair)
      })
    })
  })

  describe('getBase', () => {
    it('should return the base of the pair', () => {
      const pair = createPair('BASE' as Base, 'QUOTE' as Quote)
      if (E.isLeft(pair)) {
        throw pair.left
      }
      const base = getBase(pair.right)
      if (E.isLeft(base)) {
        throw base.left
      }
      expect(base.right).toStrictEqual('BASE' as Base)
    })
  })

  describe('getQuote', () => {
    it('should return the quote of the pair', () => {
      const pair = createPair('BASE' as Base, 'QUOTE' as Quote)
      if (E.isLeft(pair)) {
        throw pair.left
      }
      const quote = getQuote(pair.right)
      if (E.isLeft(quote)) {
        throw quote.left
      }
      expect(quote.right).toStrictEqual('QUOTE' as Quote)
    })
  })
})
