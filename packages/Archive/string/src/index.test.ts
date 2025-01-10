import { describe, expect, it } from 'vitest'

import { capitalize, isNumeric, isString, replaceSubstring, reverse } from '.'

describe('string', () => {
  describe('isString', () => {
    describe('When the value is a string', () => {
      it('should return true', () => {
        const value = 'test'
        const result = isString(value)
        expect(result).toBe(true)
      })
    })

    describe('When the value is not a string', () => {
      it('should return false', () => {
        const value = 123
        const result = isString(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('capitalize', () => {
    describe('When the input is a string', () => {
      it('should capitalize the first letter', () => {
        const value = 'test'
        const result = capitalize(value)
        expect(result).toBe('Test')
      })
    })
  })

  describe('reverse', () => {
    it('should reverse the string', () => {
      const value = 'test'
      const result = reverse(value)
      expect(result).toBe('tset')
    })

    it('should return an empty string if the input is empty', () => {
      const value = ''
      const result = reverse(value)
      expect(result).toBe('')
    })
  })

  describe('isNumeric', () => {
    describe('When the string contains only digits', () => {
      it('should return true', () => {
        const value = '123'
        const result = isNumeric(value)
        expect(result).toBe(true)
      })
    })

    describe('When the string contains non-digit characters', () => {
      it('should return false', () => {
        const value = 'abc'
        const result = isNumeric(value)
        expect(result).toBe(false)
      })
    })

    describe('When the string is empty', () => {
      it('should return false', () => {
        const value = ''
        const result = isNumeric(value)
        expect(result).toBe(false)
      })
    })
  })

  describe('replaceSubstring', () => {
    describe('When the input string is empty', () => {
      it('should return an empty string', () => {
        const actual = replaceSubstring({
          inputString: '',
          startMarker: '',
          endMarker: '',
          replacement: '',
        })
        const expected = ''
        expect(actual).toBe(expected)
      })
    })

    describe('When the input string is not empty', () => {
      describe('When the start marker is not found', () => {
        it('should return the original string', () => {
          const actual = replaceSubstring({
            inputString: 'test',
            startMarker: 'start',
            endMarker: 'end',
            replacement: 'replacement',
          })
          const expected = 'test'
          expect(actual).toBe(expected)
        })
      })

      describe('When the start marker is found', () => {
        describe('When the end marker is not found', () => {
          it('should return the original string', () => {
            const actual = replaceSubstring({
              inputString: 'starttestend',
              startMarker: 'start',
              endMarker: 'blah',
              replacement: 'replacement',
            })
            const expected = 'starttestend'
            expect(actual).toBe(expected)
          })
        })

        describe('When the end marker is found', () => {
          describe('When the replacement string is empty', () => {
            it('should return the original string', () => {
              const actual = replaceSubstring({
                inputString: 'starttestreplacementend',
                startMarker: 'start',
                endMarker: 'end',
                replacement: '',
              })
              const expected = ''
              expect(actual).toBe(expected)
            })
          })

          describe('When the replacement string is not empty', () => {
            it('should replace the substring with the replacement string', () => {
              const actual = replaceSubstring({
                inputString: 'starttestreplacementend',
                startMarker: 'start',
                endMarker: 'end',
                replacement: 'replacement',
              })
              const expected = 'replacement'
              expect(actual).toBe(expected)
            })
          })
        })
      })
    })
  })
})
