import { ChildProcessWithoutNullStreams, spawn as childProcessSpawn } from 'child_process'
import {
  either as E,
  function as FN,
  taskEither as TE,
} from 'fp-ts'

export const validateCommand = (command: string): TE.TaskEither<Error, string> =>
  FN.pipe(command, TE.fromPredicate((a) => a.length > 0, () => new Error('No command provided')))

export const splitCommand = (command: string): [string, Array<string>] => {
  const [cmd, ...args] = command.split(' ')
  if (typeof cmd === 'undefined' || cmd === '') {
    throw new Error('No command provided')
  }
  return [cmd, args]
}

export type ChildProcessProps = {
  child: ChildProcessWithoutNullStreams
  stdout: string
}

export const createChildProcess = ([cmd, args]: [string, Array<string>]): ChildProcessProps => {
  const child = childProcessSpawn(cmd, args)
  let stdout = ''

  child.stdout.on('data', (data: Uint8Array | string) => {
    process.stdout.write(data)
    stdout += String(data)
  })

  child.stderr.on('data', (data: Uint8Array | string) => {
    process.stderr.write(data)
  })

  return { child, stdout }
}

export const handleChildProcess = async ({ child, stdout }: ChildProcessProps): Promise<string> =>
  new Promise((resolve, reject) => {
    child.on('error', (error) => {
      reject(error)
    })

    child.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Command exited with code ${code}`))
      } else {
        resolve(stdout)
      }
    })
  })

export const spawn = (command: string): TE.TaskEither<Error, string> =>
  FN.pipe(command,
    validateCommand,
    TE.chain((a) => TE.tryCatch(async () => handleChildProcess(createChildProcess(splitCommand(a))), E.toError)))

export const retry = (command: string): TE.TaskEither<Error, string> =>
  FN.pipe(command, spawn, TE.orElse(() => retry(command)))
