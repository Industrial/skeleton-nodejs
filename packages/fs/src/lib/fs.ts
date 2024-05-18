import { Effect, pipe } from 'effect'
import { type Stats } from 'fs'
import fs from 'fs/promises'

export const pathExists = (path: string): Effect.Effect<Stats, Error> =>
  Effect.promise(async () =>
    fs.stat(path))

export const fileExists = (path: string): Effect.Effect<boolean, Error> =>
  pipe(
    pathExists(path),
    Effect.map((stats) =>
      stats.isFile()),
  )

export const directoryExists = (path: string): Effect.Effect<boolean, Error> =>
  pipe(
    pathExists(path),
    Effect.map((stats) =>
      stats.isDirectory()),
  )
