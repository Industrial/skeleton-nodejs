import { Effect as Fx, pipe } from 'effect'
import { type Stats } from 'fs'
import fs from 'fs/promises'

/**
 * Checks if the provided path exists.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to the file stats if the path exists, or an error otherwise.
 */
export const pathExists = (path: string): Fx.Effect<Stats, Error> =>
  Fx.promise(async () =>
    fs.stat(path))

/**
 * Checks if the provided path points to an existing file.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to true if the path exists and is a file, or false otherwise.
 */
export const fileExists = (path: string): Fx.Effect<boolean, Error> =>
  pipe(
    pathExists(path),
    Fx.map((stats) =>
      stats.isFile()),
  )

/**
 * Checks if the provided path points to an existing directory.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to true if the path exists and is a directory, or false otherwise.
 */
export const directoryExists = (path: string): Fx.Effect<boolean, Error> =>
  pipe(
    pathExists(path),
    Fx.map((stats) =>
      stats.isDirectory()),
  )
