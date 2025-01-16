import type { Abortable } from 'node:events'
import type {
  MakeDirectoryOptions,
  Mode,
  ObjectEncodingOptions,
  OpenMode,
  PathLike,
  RmDirOptions,
  StatOptions,
  Stats,
  TimeLike,
} from 'node:fs'
import fs, { type FileHandle, type FlagAndOpenMode } from 'node:fs/promises'
import type { Stream } from 'node:stream'
import { Effect as Fx, pipe } from 'effect'

/**
 * Checks if the provided path exists.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to the file stats if the path exists, or an error otherwise.
 */
export const pathExists = (path: string): Fx.Effect<Stats, Error> =>
  pipe(() => fs.stat(path), Fx.promise)

/**
 * Checks if the provided path points to an existing file.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to true if the path exists and is a file, or false otherwise.
 */
export const fileExists = (path: string): Fx.Effect<boolean, Error> => {
  console.log('fileExists', path)
  return pipe(
    pathExists(path),
    Fx.map((stats) => stats.isFile()),
  )
}

/**
 * Checks if the provided path points to an existing directory.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to true if the path exists and is a directory, or false otherwise.
 */
export const directoryExists = (path: string): Fx.Effect<boolean, Error> => {
  console.log('directoryExists', path)
  return pipe(
    pathExists(path),
    Fx.map((stats) => stats.isDirectory()),
  )
}

/**
 * Reads data from a file
 *
 * @param path - The path to the file
 * @param options - Options for reading the file
 * @returns An effect that resolves to the file content
 */
export const readFile = (
  path: string,
  options?:
    | ({
        encoding?: null | undefined
        flag?: OpenMode | undefined
      } & Abortable)
    | null,
): Fx.Effect<string | Buffer, Error> =>
  pipe(() => fs.readFile(path, options), Fx.promise)

/**
 * Writes data to a file
 *
 * @param path - The path to the file
 * @param data - The data to write
 * @param options - Options for writing the file
 * @returns An effect that resolves when the operation is complete
 */
export const writeFile = (
  file: PathLike | FileHandle,
  data:
    | string
    | NodeJS.ArrayBufferView
    | Iterable<string | NodeJS.ArrayBufferView>
    | AsyncIterable<string | NodeJS.ArrayBufferView>
    | Stream,
  options?:
    | (ObjectEncodingOptions & {
        mode?: Mode | undefined
        flag?: OpenMode | undefined
        /**
         * If all data is successfully written to the file, and `flush`
         * is `true`, `filehandle.sync()` is used to flush the data.
         * @default false
         */
        flush?: boolean | undefined
      } & Abortable)
    | BufferEncoding
    | null,
): Fx.Effect<void, Error> =>
  pipe(() => fs.writeFile(file, data, options), Fx.promise)

/**
 * Appends data to a file
 *
 * @param path - The path to the file
 * @param data - The data to append
 * @param options - Options for appending the file
 * @returns An effect that resolves when the operation is complete
 */
export const appendFile = (
  path: PathLike | FileHandle,
  data: string | Uint8Array,
  options?:
    | (ObjectEncodingOptions &
        FlagAndOpenMode & { flush?: boolean | undefined })
    | BufferEncoding
    | null,
): Fx.Effect<void, Error> =>
  pipe(() => fs.appendFile(path, data, options), Fx.promise)

/**
 * Deletes a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves when the operation is complete
 */
export const unlink = (path: string): Fx.Effect<void, Error> =>
  pipe(() => fs.unlink(path), Fx.promise)

/**
 * Renames a file
 *
 * @param oldPath - The current path of the file
 * @param newPath - The new path of the file
 * @returns An effect that resolves when the operation is complete
 */
export const rename = (
  oldPath: string,
  newPath: string,
): Fx.Effect<void, Error> => pipe(() => fs.rename(oldPath, newPath), Fx.promise)

/**
 * Creates a new directory
 *
 * @param path - The path to the directory
 * @param options - Options for creating the directory
 * @returns An effect that resolves when the operation is complete
 */
export const mkdir = (
  path: PathLike,
  options: MakeDirectoryOptions & {
    recursive: true
  },
): Fx.Effect<void, Error> => pipe(() => fs.mkdir(path, options), Fx.promise)

/**
 * Reads the contents of a directory
 *
 * @param path - The path to the directory
 * @param options - Options for reading the directory
 * @returns An effect that resolves to the directory contents
 */
export const readdir = (
  path: PathLike,
  options?:
    | (ObjectEncodingOptions & {
        withFileTypes?: false | undefined
        recursive?: boolean | undefined
      })
    | BufferEncoding
    | null,
): Fx.Effect<string[], Error> =>
  pipe(() => fs.readdir(path, options), Fx.promise)

/**
 * Removes a directory
 *
 * @param path - The path to the directory
 * @returns An effect that resolves when the operation is complete
 */
export const rmdir = (
  path: PathLike,
  options?: RmDirOptions,
): Fx.Effect<void, Error> => pipe(() => fs.rmdir(path, options), Fx.promise)

/**
 * Truncates a file to a specified length
 *
 * @param path - The path to the file
 * @param len - The length to truncate to
 * @returns An effect that resolves when the operation is complete
 */
export const truncate = (
  path: PathLike,
  len?: number,
): Fx.Effect<void, Error> => pipe(() => fs.truncate(path, len), Fx.promise)

/**
 * Changes the permissions of a file
 *
 * @param path - The path to the file
 * @param mode - The file mode
 * @returns An effect that resolves when the operation is complete
 */
export const chmod = (path: PathLike, mode: Mode): Fx.Effect<void, Error> =>
  pipe(() => fs.chmod(path, mode), Fx.promise)

/**
 * Changes the owner of a file
 *
 * @param path - The path to the file
 * @param uid - The user ID
 * @param gid - The group ID
 * @returns An effect that resolves when the operation is complete
 */
export const chown = (
  path: PathLike,
  uid: number,
  gid: number,
): Fx.Effect<void, Error> => pipe(() => fs.chown(path, uid, gid), Fx.promise)

/**
 * Reads the stats of a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves to the file stats
 */
export const stat = (
  path: PathLike,
  opts?: StatOptions & {
    bigint?: false | undefined
  },
): Fx.Effect<Stats, Error> => pipe(() => fs.stat(path, opts), Fx.promise)

/**
 * Reads the symbolic link stats of a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves to the file stats
 */
export const lstat = (
  path: PathLike,
  opts?: StatOptions & {
    bigint?: false | undefined
  },
): Fx.Effect<Stats, Error> => pipe(() => fs.lstat(path, opts), Fx.promise)

/**
 * Creates a symbolic link
 *
 * @param target - The path to the target
 * @param path - The path to the symbolic link
 * @returns An effect that resolves when the operation is complete
 */
export const symlink = (
  target: PathLike,
  path: PathLike,
  type?: string | null,
): Fx.Effect<void, Error> =>
  pipe(() => fs.symlink(target, path, type), Fx.promise)

/**
 * Creates a hard link
 *
 * @param existingPath - The path to the existing file
 * @param newPath - The path to the new link
 * @returns An effect that resolves when the operation is complete
 */
export const link = (
  existingPath: PathLike,
  newPath: PathLike,
): Fx.Effect<void, Error> =>
  pipe(() => fs.link(existingPath, newPath), Fx.promise)

/**
 * Reads the value of a symbolic link
 *
 * @param path - The path to the symbolic link
 * @returns An effect that resolves to the link value
 */
export const readlink = (
  path: PathLike,
  options?: ObjectEncodingOptions | BufferEncoding | null,
): Fx.Effect<string, Error> =>
  pipe(() => fs.readlink(path, options), Fx.promise)

/**
 * Resolves a path to an absolute path
 *
 * @param path - The path to resolve
 * @returns An effect that resolves to the absolute path
 */
export const realpath = (
  path: PathLike,
  options?: ObjectEncodingOptions | BufferEncoding | null,
): Fx.Effect<string, Error> =>
  pipe(() => fs.realpath(path, options), Fx.promise)

/**
 * Copies a file
 *
 * @param src - The path to the source file
 * @param dest - The path to the destination file
 * @param flags - Optional copy operation flags
 * @returns An effect that resolves when the operation is complete
 */
export const copyFile = (
  src: PathLike,
  dest: PathLike,
  mode?: number,
): Fx.Effect<void, Error> =>
  pipe(() => fs.copyFile(src, dest, mode), Fx.promise)

/**
 * Changes the timestamps of a file
 *
 * @param path - The path to the file
 * @param atime - The new access time
 * @param mtime - The new modification time
 * @returns An effect that resolves when the operation is complete
 */
export const utimes = (
  path: PathLike,
  atime: TimeLike,
  mtime: TimeLike,
): Fx.Effect<void, Error> =>
  pipe(() => fs.utimes(path, atime, mtime), Fx.promise)
