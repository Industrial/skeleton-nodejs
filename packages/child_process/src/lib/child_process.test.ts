import { either as E } from 'fp-ts'
import { describe, expect, it } from 'vitest'

import { spawn } from './child_process.ts'

describe('child_process', () => {
  describe('spawn', () => {
    describe('When called with an empty string', () => {
      it('should return an error', async () => {
        const result = await spawn('')()
        expect(E.isLeft(result)).toBeTruthy()
      })
    })

    describe('When called with a command', () => {
      it('should return the result of the command', async () => {
        const result = await spawn('echo hello')()
        expect(E.isRight(result)).toBeTruthy()
      })
    })
  })
})

