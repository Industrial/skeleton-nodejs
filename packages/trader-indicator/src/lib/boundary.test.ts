import { describe, expect, it } from 'bun:test'

import { hasCrossedBoundaryDownwardsAtIndex, hasCrossedBoundaryUpwardsAtIndex } from './boundary.ts'

describe('boundary', () => {
  describe('hasCrossedBoundaryUpwardsAtIndex', () => {
    describe('When value before the index is undefined', () => {
      it('should return false', () => {
        const values = [1, 2, 3]
        const boundary = 1
        const index = 0
        expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(false)
      })
    })
    describe('When the value before the index is defined', () => {
      describe('When the value at the index is undefined', () => {
        it('should return false', () => {
          const values = [1, 2, 3]
          const boundary = 1
          const index = 3
          expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(false)
        })
      })
      describe('When the value at the index is defined', () => {
        describe('When the value before the index is higher then the boundary', () => {
          it('should return false', () => {
            const values = [1, 2, 3]
            const boundary = 1
            const index = 2
            expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(false)
          })
        })
        describe('When the value before the index is lower then the boundary', () => {
          describe('When the value at the index is lower then the boundary', () => {
            it('should return false', () => {
              const values = [1, 2, 3]
              const boundary = 4
              const index = 2
              expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(false)
            })
          })
          describe('When the value at the index is equal to the boundary', () => {
            it('should return true', () => {
              const values = [1, 2, 3]
              const boundary = 3
              const index = 2
              expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(true)
            })
          })
          describe('When the value at the index is higher then the boundary', () => {
            it('should return true', () => {
              const values = [1, 2, 4]
              const boundary = 3
              const index = 2
              expect(hasCrossedBoundaryUpwardsAtIndex(values, boundary, index)).toEqual(true)
            })
          })
        })
      })
    })
  })

  describe('hasCrossedBoundaryDownwardsAtIndex', () => {
    describe('When value before the index is undefined', () => {
      it('should return false', () => {
        const values = [3, 2, 1]
        const boundary = 1
        const index = 0
        expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(false)
      })
    })
    describe('When the value before the index is defined', () => {
      describe('When the value at the index is undefined', () => {
        it('should return false', () => {
          const values = [3, 2, 1]
          const boundary = 1
          const index = 3
          expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(false)
        })
      })
      describe('When the value at the index is defined', () => {
        describe('When the value before the index is lower then the boundary', () => {
          it('should return false', () => {
            const values = [3, 2, 1]
            const boundary = 3
            const index = 2
            expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(false)
          })
        })
        describe('When the value before the index is higher then the boundary', () => {
          describe('When the value at the index is higher then the boundary', () => {
            it('should return false', () => {
              const values = [3, 2, 1]
              const boundary = 4
              const index = 2
              expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(false)
            })
          })
          describe('When the value at the index is equal to the boundary', () => {
            it('should return true', () => {
              const values = [3, 2, 1]
              const boundary = 1
              const index = 2
              expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(true)
            })
          })
          describe('When the value at the index is lower then the boundary', () => {
            it('should return true', () => {
              const values = [3, 2, 0]
              const boundary = 1
              const index = 2
              expect(hasCrossedBoundaryDownwardsAtIndex(values, boundary, index)).toEqual(true)
            })
          })
        })
      })
    })
  })
})
