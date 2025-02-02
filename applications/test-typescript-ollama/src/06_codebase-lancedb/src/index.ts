import type { Dirent } from 'node:fs'
import fs from 'node:fs/promises'
import path from 'node:path'
import { LanceDB } from '@langchain/community/vectorstores/lancedb'
import type { Document } from '@langchain/core/documents'
import { ChatPromptTemplate } from '@langchain/core/prompts'
import type { Runnable } from '@langchain/core/runnables'
import { Ollama, OllamaEmbeddings } from '@langchain/ollama'
import { Array as A, Effect as E, LogLevel, Logger, pipe } from 'effect'
import type { UnknownException } from 'effect/Cause'
import { createRetrievalChain as langChainCreateRetrievalChain } from 'langchain/chains/retrieval'
import { TextLoader } from 'langchain/document_loaders/fs/text'
import { RecursiveCharacterTextSplitter } from 'langchain/text_splitter'

const ollamaHost = '0.0.0.0'
const ollamaPort = 11434
const ollamaEmbeddingsModel = 'mxbai-embed-large:latest'
const ollamaRetrievalModel = 'deepseek-r1:32b'
const collectionName = '06_codebase-lancedb'

const sourceDirectory = path.join(__dirname, '..', 'src')

const writeResponseToFile = (response: Answer): Promise<void> =>
  fs.writeFile('output.md', response.answer)

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

const readDir = (directoryPath: string): Promise<Dirent[]> =>
  fs.readdir(directoryPath, { withFileTypes: true })

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
): E.Effect<DocumentWithMetadata[], UnknownException, never> =>
  E.gen(function* (_) {
    const textLoader = new TextLoader(filePath)
    const textSplitter = new RecursiveCharacterTextSplitter({
      chunkSize: CHUNK_SIZE,
      chunkOverlap: CHUNK_OVERLAP,
    })

    const loadedTexts = yield* _(E.tryPromise(() => textLoader.load()))

    const splitDocuments = yield* _(
      E.tryPromise(() => textSplitter.splitDocuments(loadedTexts)),
    )

    return addMetadata(filePath)(splitDocuments)
  })

const listFilesRecursively = (
  directoryPath: string,
): E.Effect<string[], UnknownException, never> =>
  E.gen(function* (_) {
    const entries = yield* _(
      E.promise(() => readDir(path.resolve(directoryPath))),
    )

    const results = yield* E.all(
      entries.map((entry) =>
        entry.isDirectory()
          ? listFilesRecursively(path.join(directoryPath, entry.name))
          : E.succeed([path.join(directoryPath, entry.name)]),
      ),
    )

    return results.flat()
  })

export const importDocuments = (directory: string) =>
  E.gen(function* (_) {
    const filePaths = yield* _(listFilesRecursively(directory))
    const documents = yield* _(E.all(filePaths.map(processFile)))
    return documents.flat()
  })

export const askQuestionToRetrievalChain =
  (question: string, targetFilePath?: string) =>
  (retrievalChain: Runnable): Promise<Answer> =>
    retrievalChain.invoke({
      input: question,
      metadata: targetFilePath
        ? {
            queryFilePath: targetFilePath,
          }
        : {},
    })

const askQuestion = (question: Question, targetFilePath?: string) => {
  return E.gen(function* (_) {
    yield* E.logDebug({ question })

    const embeddingsModel = new OllamaEmbeddings({
      baseUrl: `http://${ollamaHost}:${ollamaPort}`,
      model: ollamaEmbeddingsModel,
    })
    const retrievalModel = new Ollama({
      baseUrl: `http://${ollamaHost}:${ollamaPort}`,
      model: ollamaRetrievalModel,
    })
    const vectorStore = new LanceDB(embeddingsModel, {
      mode: 'overwrite',
      tableName: collectionName,
    })
    const retriever = vectorStore.asRetriever({
      k: 9999,
      filter: filterDocumentByFilePath,
      searchType: 'similarity',
    })

    const chatPromptTemplate = `
		You are a helpful coding assistant. Use the following source code context to answer the question.
		Each piece of context has a source file path in its metadata - only use context from relevant files.

		Context from source files:
			{context}

		Question or task to address: 
			{input}

		When providing code examples or suggestions, ensure they are consistent with the specific file's context and patterns.
  `

    yield* E.logDebug('Importing documents...')
    const documents = yield* _(importDocuments(sourceDirectory))
    yield* E.logDebug(`Imported ${documents.length} documents`)

    yield* E.logDebug('Adding documents to vector store...')
    yield* _(E.promise(() => vectorStore.addDocuments(documents)))
    yield* E.logDebug('Added documents to vector store')

    yield* E.logDebug('Creating retrieval chain...')

    const retrievalChain = yield* _(
      E.promise(() =>
        langChainCreateRetrievalChain({
          combineDocsChain:
            ChatPromptTemplate.fromTemplate(chatPromptTemplate).pipe(
              retrievalModel,
            ),
          retriever,
        }),
      ),
    )

    yield* E.logDebug('Asking question to retrieval chain...')
    const answer = yield* _(
      E.promise(() =>
        askQuestionToRetrievalChain(question, targetFilePath)(retrievalChain),
      ),
    )

    yield* E.logDebug('Writing response to file...')
    yield* _(E.promise(() => writeResponseToFile(answer)))

    yield* E.logDebug('Logging answer.')
    yield* E.logInfo(answer)

    return answer
  })
}

const question: Question = `
Describe the documentation in the docs directory.
`

await E.runPromise(
  askQuestion(question).pipe(Logger.withMinimumLogLevel(LogLevel.Debug)),
)
