import { either as E } from 'fp-ts'
import { describe, expect, it } from 'vitest'

import { sequenceArrayWritable } from './either.ts'

describe('Either', () => {
  describe('sequenceArrayWritable', () => {
    describe('When called with an array with one Left', () => {
      it('should return a Left of that value', () => {
        const as: Array<E.Either<Error, string>> = [E.left(new Error('a'))]
        const actual = sequenceArrayWritable(as)
        if (E.isRight(actual)) {
          throw new Error('Did not expect a right')
        }
        expect(actual.left.message).toBe('a')
      })
    })

    describe('When called with an empty array', () => {
      it('should return an array with a single Right of an empty array', () => {
        const as: Array<E.Either<Error, string>> = []
        const actual = sequenceArrayWritable(as)
        if (E.isLeft(actual)) {
          throw new Error('Did not expect an error')
        }
        expect(actual.right).toStrictEqual([])
      })
    })

    describe('When called with an array with one Right', () => {
      it('should return a Right of an array with that value', () => {
        const as: Array<E.Either<Error, string>> = [E.right('a')]
        const actual = sequenceArrayWritable(as)
        if (E.isLeft(actual)) {
          throw new Error('Did not expect an error')
        }
        expect(actual.right).toStrictEqual(['a'])
      })
    })
  })
})
