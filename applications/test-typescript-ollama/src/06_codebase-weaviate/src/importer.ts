import type { Dirent } from 'node:fs'
import fs from 'node:fs/promises'
import path from 'node:path'
import type { Document } from '@langchain/core/documents'
import { Array as A, Effect as E, pipe } from 'effect'
import type { UnknownException } from 'effect/Cause'
import { TextLoader } from 'langchain/document_loaders/fs/text'
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter'

const CHUNK_SIZE = 500
const CHUNK_OVERLAP = 100

export type FilePathMetadata = {
  filePath: string
}

export type QueryFilePathMetadata = {
  queryFilePath?: string
}

export type DocumentWithMetadata = Document & {
  metadata: FilePathMetadata & QueryFilePathMetadata
}

export type Question = string

export type Answer = {
  answer: string
}

export const filterDocumentByFilePath = (doc: DocumentWithMetadata) =>
  doc.metadata.queryFilePath
    ? doc.metadata.filePath === doc.metadata.queryFilePath
    : true

export const addMetadata =
  (filePath: string) =>
  (documents: Document[]): DocumentWithMetadata[] =>
    pipe(
      documents,
      A.map((doc) => ({
        ...doc,
        metadata: {
          ...doc.metadata,
          // Remove the __dirname prefix and add a leading slash
          filePath: filePath.replace(__dirname, '').replace(/^\//, ''),
        } satisfies FilePathMetadata,
      })),
    )

export const processFile = (
  filePath: string,
): E.Effect<DocumentWithMetadata[], UnknownException, never> => {
  const textLoader = new TextLoader(filePath)
  const textSplitter = new RecursiveCharacterTextSplitter({
    chunkSize: CHUNK_SIZE,
    chunkOverlap: CHUNK_OVERLAP,
  })

  return pipe(
    E.tryPromise(() => textLoader.load()),
    E.flatMap((loadedTexts) =>
      E.tryPromise(() => textSplitter.splitDocuments(loadedTexts)),
    ),
    E.map(addMetadata(filePath)),
  )
}

const readDir = (directoryPath: string): Promise<Dirent[]> =>
  fs.readdir(directoryPath, { withFileTypes: true })

export const flattenArray = <A>(a: A[][]): A[] => a.flat()

export const listFilesRecursively = (
  directoryPath: string,
): E.Effect<string[], UnknownException, never> =>
  pipe(
    E.tryPromise(() => readDir(directoryPath)),
    E.flatMap((entries) =>
      pipe(
        entries,
        A.map((entry) =>
          entry.isDirectory()
            ? listFilesRecursively(path.join(directoryPath, entry.name))
            : E.succeed([path.join(directoryPath, entry.name)]),
        ),
        E.all,
        E.map(flattenArray),
      ),
    ),
  )

export const importDocuments = (
  directory: string,
): E.Effect<DocumentWithMetadata[], UnknownException, never> =>
  pipe(
    listFilesRecursively(directory),
    E.flatMap((filePaths) =>
      pipe(filePaths, A.map(processFile), E.all, E.map(flattenArray)),
    ),
  )
