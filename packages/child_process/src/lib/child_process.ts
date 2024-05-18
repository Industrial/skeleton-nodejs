import { createReadableStreamProperty, nodeReadableToReadableStream } from '@code9/stream'
import { type Property } from '@frp-ts/core'
import { spawn, SpawnOptionsWithoutStdio } from 'child_process'
import { either as E } from 'fp-ts'

export const createChildProcess = <T>(command: string, args?: Array<string>, options?: SpawnOptionsWithoutStdio): {
  stdout: Property<E.Either<Error, Uint8Array>>
  stderr: Property<E.Either<Error, Uint8Array>>
} => {
  const process = spawn(command, args, options)

  const stdoutReadableStream = nodeReadableToReadableStream(process.stdout)
  const stdoutProperty = createReadableStreamProperty(stdoutReadableStream)

  const stderrReadableStream = nodeReadableToReadableStream(process.stderr)
  const stderrProperty = createReadableStreamProperty(stderrReadableStream)

  return {
    stdout: stdoutProperty,
    stderr: stderrProperty,
  }
}
