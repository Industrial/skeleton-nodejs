import { Effect as Fx, pipe } from 'effect'
import type { Stats } from 'fs'
import fs from 'fs/promises'

/**
 * Checks if the provided path exists.
 *
 * @param path - The path to check.
 * @returns An effect that resolves to the file stats if the path exists, or an error otherwise.
 */
export const pathExists = (path: string): Fx.Effect<Stats, Error> => pipe(() => fs.stat(path), Fx.promise)

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
export const readFile = (path: string, options?: any): Fx.Effect<string | Buffer, Error> =>
  pipe(() => fs.readFile(path, options), Fx.promise)

/**
 * Writes data to a file
 *
 * @param path - The path to the file
 * @param data - The data to write
 * @param options - Options for writing the file
 * @returns An effect that resolves when the operation is complete
 */
export const writeFile = (path: string, data: string | Buffer, options?: any): Fx.Effect<void, Error> =>
  pipe(() => fs.writeFile(path, data, options), Fx.promise)

/**
 * Appends data to a file
 *
 * @param path - The path to the file
 * @param data - The data to append
 * @param options - Options for appending the file
 * @returns An effect that resolves when the operation is complete
 */
export const appendFile = (path: string, data: string | Buffer, options?: any): Fx.Effect<void, Error> =>
  pipe(() => fs.appendFile(path, data, options), Fx.promise)

/**
 * Deletes a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves when the operation is complete
 */
export const unlink = (path: string): Fx.Effect<void, Error> => pipe(() => fs.unlink(path), Fx.promise)

/**
 * Renames a file
 *
 * @param oldPath - The current path of the file
 * @param newPath - The new path of the file
 * @returns An effect that resolves when the operation is complete
 */
export const rename = (oldPath: string, newPath: string): Fx.Effect<void, Error> =>
  pipe(() => fs.rename(oldPath, newPath), Fx.promise)

/**
 * Creates a new directory
 *
 * @param path - The path to the directory
 * @param options - Options for creating the directory
 * @returns An effect that resolves when the operation is complete
 */
export const mkdir = (path: string, options?: any): Fx.Effect<void, Error> => pipe(() => fs.mkdir(path, options), Fx.promise)

/**
 * Reads the contents of a directory
 *
 * @param path - The path to the directory
 * @param options - Options for reading the directory
 * @returns An effect that resolves to the directory contents
 */
export const readdir = (path: string, options?: any): Fx.Effect<string[], Error> =>
  pipe(() => fs.readdir(path, options), Fx.promise)

/**
 * Removes a directory
 *
 * @param path - The path to the directory
 * @returns An effect that resolves when the operation is complete
 */
export const rmdir = (path: string): Fx.Effect<void, Error> => pipe(() => fs.rmdir(path), Fx.promise)

/**
 * Truncates a file to a specified length
 *
 * @param path - The path to the file
 * @param len - The length to truncate to
 * @returns An effect that resolves when the operation is complete
 */
export const truncate = (path: string, len?: number): Fx.Effect<void, Error> => pipe(() => fs.truncate(path, len), Fx.promise)

/**
 * Changes the permissions of a file
 *
 * @param path - The path to the file
 * @param mode - The file mode
 * @returns An effect that resolves when the operation is complete
 */
export const chmod = (path: string, mode: string | number): Fx.Effect<void, Error> => pipe(() => fs.chmod(path, mode), Fx.promise)

/**
 * Changes the owner of a file
 *
 * @param path - The path to the file
 * @param uid - The user ID
 * @param gid - The group ID
 * @returns An effect that resolves when the operation is complete
 */
export const chown = (path: string, uid: number, gid: number): Fx.Effect<void, Error> =>
  pipe(() => fs.chown(path, uid, gid), Fx.promise)

/**
 * Reads the stats of a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves to the file stats
 */
export const stat = (path: string): Fx.Effect<Stats, Error> => pipe(() => fs.stat(path), Fx.promise)

/**
 * Reads the symbolic link stats of a file
 *
 * @param path - The path to the file
 * @returns An effect that resolves to the file stats
 */
export const lstat = (path: string): Fx.Effect<Stats, Error> => pipe(() => fs.lstat(path), Fx.promise)

/**
 * Creates a symbolic link
 *
 * @param target - The path to the target
 * @param path - The path to the symbolic link
 * @returns An effect that resolves when the operation is complete
 */
export const symlink = (target: string, path: string): Fx.Effect<void, Error> => pipe(() => fs.symlink(target, path), Fx.promise)

/**
 * Creates a hard link
 *
 * @param existingPath - The path to the existing file
 * @param newPath - The path to the new link
 * @returns An effect that resolves when the operation is complete
 */
export const link = (existingPath: string, newPath: string): Fx.Effect<void, Error> =>
  pipe(() => fs.link(existingPath, newPath), Fx.promise)

/**
 * Reads the value of a symbolic link
 *
 * @param path - The path to the symbolic link
 * @returns An effect that resolves to the link value
 */
export const readlink = (path: string): Fx.Effect<string, Error> => pipe(() => fs.readlink(path), Fx.promise)

/**
 * Resolves a path to an absolute path
 *
 * @param path - The path to resolve
 * @returns An effect that resolves to the absolute path
 */
export const realpath = (path: string): Fx.Effect<string, Error> => pipe(() => fs.realpath(path), Fx.promise)

/**
 * Copies a file
 *
 * @param src - The path to the source file
 * @param dest - The path to the destination file
 * @param flags - Optional copy operation flags
 * @returns An effect that resolves when the operation is complete
 */
export const copyFile = (src: string, dest: string, flags?: number): Fx.Effect<void, Error> =>
  pipe(() => fs.copyFile(src, dest, flags), Fx.promise)

/**
 * Changes the timestamps of a file
 *
 * @param path - The path to the file
 * @param atime - The new access time
 * @param mtime - The new modification time
 * @returns An effect that resolves when the operation is complete
 */
export const utimes = (path: string, atime: string | number | Date, mtime: string | number | Date): Fx.Effect<void, Error> =>
  pipe(() => fs.utimes(path, atime, mtime), Fx.promise)
