#!/usr/bin/env bun
import * as A from 'fp-ts/Array'
import * as E from 'fp-ts/Either'
import * as FN from 'fp-ts/function'
import * as S from 'fp-ts/string'
import * as TE from 'fp-ts/TaskEither'
import { readdir, stat } from 'fs/promises'
import { spawnChildProcessStream } from 'packages/child_process/src/index.ts'

const packageJSON = await import('../package.json')
// @ts-expect-error workspaces is optional
const packageJSONWorkspaces: Array<string> = packageJSON.workspaces ?? []

const removeStar = (workspacePattern: string): string =>
  workspacePattern.replace(/\/\*$/u, '')

const readdirTE = (dirPath: string): TE.TaskEither<Error, Array<string>> =>
  TE.tryCatch(async () =>
    readdir(dirPath, { recursive: false }), E.toError)

const isDirectoryTE = (entry: string): TE.TaskEither<Error, boolean> =>
  TE.tryCatch(async () =>
    (await stat(entry)).isDirectory(), E.toError)

const getEntryPath = (a: string, entry: string): string =>
  `${a}/${entry}`

const getEntryPathTE = (a: string, entry: string): TE.TaskEither<Error, string> =>
  FN.pipe(
    getEntryPath(a, entry),
    isDirectoryTE,
    TE.map(() =>
      getEntryPath(a, entry)),
  )

const getEntryPathsTE = (a: string): TE.TaskEither<Error, Array<string>> =>
  FN.pipe(
    removeStar(a),
    readdirTE,
    TE.chain(A.traverse(TE.ApplicativePar)((entry) =>
      getEntryPathTE(removeStar(a), entry))),
  )

const getWorkspacePaths = (workspacePattern: string): TE.TaskEither<Error, Array<string>> =>
  workspacePattern.endsWith('/*')
    ? getEntryPathsTE(workspacePattern)
    : TE.right([workspacePattern])

const getWorkspaces = (workspaces: Array<string>): TE.TaskEither<Error, Array<string>> =>
  FN.pipe(
    workspaces,
    A.traverse(TE.ApplicativePar)(getWorkspacePaths),
    TE.map(A.flatten),
    TE.map(A.uniq(S.Ord)),
    TE.map(A.sort(S.Ord)),
  )

const cmd = process.argv.slice(2).join(' ')

// const result = await getWorkspaces(packageJSONWorkspaces)()
// console.log('result', result)

const main = async () =>
  FN.pipe(
    packageJSONWorkspaces,
    getWorkspaces,
    TE.chain(A.traverse(TE.ApplicativePar)((entry) =>
      spawnChildProcessStream(cmd, [entry], {
        cwd: entry,
        shell: true,
      }))),
  )()

const x = await spawnChildProcessStream('bun', ['run', 'test'], {
  cwd: 'applications/solid-app',
  shell: true,
})()

try {
  await main()
} catch (error: unknown) {
  console.error(error)
}
