import { either as E, function as FN, taskEither as TE } from 'fp-ts'
import type { Stats } from 'fs'
import fs from 'fs/promises'

export const pathExists = async (path: string, predicate: (stats: Stats) => boolean): Promise<boolean> =>
  FN.pipe(
    TE.tryCatch(async () =>
      fs.stat(path), E.toError),
    TE.match(() =>
      false, predicate),
  )()

export const fileExists = async (path: string): Promise<boolean> =>
  pathExists(path, (stats) =>
    stats.isFile())

export const directoryExists = async (path: string): Promise<boolean> =>
  pathExists(path, (stats) =>
    stats.isDirectory())
