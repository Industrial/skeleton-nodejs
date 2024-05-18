import { describe, expect, it } from 'bun:test'
import { isLeft, isRight } from 'fp-ts/Either'

import { createPair, getBase, getQuote, isCorrectBaseE, isCorrectPairFormatE, isCorrectQuoteE } from '../lib/pair.ts'

describe('Pair module', () => {
  describe('isCorrectBaseE', () => {
    describe('When base is an empty string', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains lower case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('abc')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains characters other then upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('ABC@!#$')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectBaseE('ABC')
        expect(isRight(actual)).to.be.equal(true)
      })
    })
  })

  describe('isCorrectQuoteE', () => {
    describe('When base is an empty string', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains lower case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('abc')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains characters other then upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('ABC@!#$')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base contains upper case characters', () => {
      it('should return an error', () => {
        const actual = isCorrectQuoteE('ABC')
        expect(isRight(actual)).to.be.equal(true)
      })
    })
  })

  describe('isCorrectPairFormat', () => {
    describe('When format has a length lower then 2', () => {
      it('should return an error', () => {
        const actual = isCorrectPairFormatE(['BASE'])
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When format has a length higher or equal then 2', () => {
      it('should return the array', () => {
        const actual = isCorrectPairFormatE(['BASE', 'QUOTE'])
        if (isLeft(actual)) {
          throw actual.left
        }
        expect(isRight(actual)).toBe(true)
        expect(actual.right).to.be.deep.equal(['BASE', 'QUOTE'])
      })
    })
  })

  describe('createPair', () => {
    describe('When base is empty', () => {
      it('should return an error', () => {
        const actual = createPair('', 'QUOTE')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When quote is empty', () => {
      it('should return an error', () => {
        const actual = createPair('BASE', '')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base is not uppercase', () => {
      it('should return an error', () => {
        const actual = createPair('base', 'QUOTE')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When quote is not uppercase', () => {
      it('should return an error', () => {
        const actual = createPair('BASE', 'quote')
        expect(isLeft(actual)).to.be.equal(true)
      })
    })

    describe('When base and quote are uppercase', () => {
      it('should create a pair', () => {
        const actual = createPair('BASE', 'QUOTE')
        if (isLeft(actual)) {
          throw actual.left
        }
        expect(actual.right).to.be.equal(`BASE/QUOTE`)
      })
    })
  })

  describe('getBase', () => {
    it('should return the base of the pair', () => {
      const pair = createPair('BASE', 'QUOTE')
      if (isLeft(pair)) {
        throw pair.left
      }
      const base = getBase(pair.right)
      if (isLeft(base)) {
        throw base.left
      }
      expect(base.right).to.be.equal('BASE')
    })
  })

  describe('getQuote', () => {
    it('should return the quote of the pair', () => {
      const pair = createPair('BASE', 'QUOTE')
      if (isLeft(pair)) {
        throw pair.left
      }
      const quote = getQuote(pair.right)
      if (isLeft(quote)) {
        throw quote.left
      }
      expect(quote.right).to.be.equal('QUOTE')
    })
  })
})
