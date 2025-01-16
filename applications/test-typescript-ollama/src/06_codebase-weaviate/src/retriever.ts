import type { LanceDB } from '@langchain/community/vectorstores/lancedb'
import { ChatPromptTemplate } from '@langchain/core/prompts'
import type { Runnable } from '@langchain/core/runnables'
import type { VectorStoreRetriever } from '@langchain/core/vectorstores'
import type { Ollama } from '@langchain/ollama'
import { createRetrievalChain as langChainCreateRetrievalChain } from 'langchain/chains/retrieval'
import type { Answer } from './importer'

export const createRetrievalChain = (
  chatPromptTemplate: string,
  retrievalModel: Ollama,
  retriever: VectorStoreRetriever<LanceDB>,
): Promise<Runnable> =>
  langChainCreateRetrievalChain({
    combineDocsChain:
      ChatPromptTemplate.fromTemplate(chatPromptTemplate).pipe(retrievalModel),
    retriever,
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
