import { ChildProcessWithoutNullStreams, spawn as childProcessSpawn } from 'child_process'
import { either as E, function as FN, taskEither as TE } from 'fp-ts'

export const validateCommand = (command: string): TE.TaskEither<Error, string> =>
  FN.pipe(
    command,
    TE.fromPredicate(
      (a) =>
        a.length > 0,
      () =>
        new Error('No command provided'),
    ),
  )

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

export const spawnChildProcess = ([cmd, args]: [string, Array<string>]): ChildProcessProps => {
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
    child.once('error', (error) => {
      reject(error)
    })

    child.once('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Command exited with code ${code}`))
      } else {
        resolve(stdout)
      }
    })
  })

export const spawn = (command: string): TE.TaskEither<Error, void> =>
  FN.pipe(
    command,
    validateCommand,
    TE.chain((a) =>
      TE.tryCatch(
        async () =>
          new Promise<void>((resolve, reject) => {
            const process = childProcessSpawn(a, {
              shell: true,
              stdio: 'inherit',
            })

            process.once('error', (error) => {
              reject(error)
            })

            process.once('exit', (code) => {
              if (code !== 0) {
                reject(new Error(`Command "${command}" exited with code ${code}`))
              } else {
                resolve(undefined)
              }
            })
          }),
        E.toError,
      )),
  )

export const retrySpawn = (command: string): TE.TaskEither<Error, void> =>
  FN.pipe(
    command,
    spawn,
    TE.orElse(() =>
      retrySpawn(command)),
  )
